use std::rc::Rc;

use paste::paste;
use rug::Integer;
use syn::{
    bracketed, parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::{Bracket, Paren},
    Lit, LitStr, Result, Token,
};

use crate::ast::{
    kw, type_kind::parse_type, utilities::peek_and_consume, Ident, QualifiedName, TypeKind,
    TypeParameters,
};

use super::utilities::peek_and_consume_ellipsis;

#[derive(Debug, Clone, Copy)]
enum Operator {
    CopyAndUpdate,        // w/ <- | ternary | left   |  1 |
    Range,                // ..    | infix   | left   |  2 |
    Conditional,          // ? |   | ternary | right  |  5 |
    LogicalOr,            // or    | infix   | left   | 10 |
    LogicalOrDeprecated,  // ||    | infix   | left   | 10 |
    LogicalAnd,           // and   | infix   | left   | 11 |
    LogicalAndDeprecated, // &&    | infix   | left   | 11 |
    BitwiseOr,            // |||   | infix   | left   | 12 |
    BitwiseXor,           // ^^^   | infix   | left   | 13 |
    BitwiseAnd,           // &&&   | infix   | left   | 14 |
    Equality,             // ==    | infix   | left   | 20 |
    Inequality,           // !=    | infix   | left   | 20 |
    LessThanOrEqual,      // <=    | infix   | left   | 25 |
    LessThan,             // <     | infix   | left   | 25 |
    GreaterThanOrEqual,   // >=    | infix   | left   | 25 |
    GreaterThan,          // >     | infix   | left   | 25 |
    RightShift,           // >>>   | infix   | left   | 28 |
    LeftShift,            // <<<   | infix   | left   | 28 |
    Addition,             // +     | infix   | left   | 30 |
    Subtraction,          // -     | infix   | left   | 30 |
    Multiplication,       // *     | infix   | left   | 35 |
    Division,             // /     | infix   | left   | 35 |
    Modulus,              // %     | infix   | left   | 35 |
    Exponentiation,       // ^     | infix   | right  | 40 |
}

impl Operator {
    // Rust implements these match statements in terms of a jump table
    pub fn precedence(&self) -> u32 {
        match self {
            Operator::CopyAndUpdate => 1,
            Operator::Range => 2,
            Operator::Conditional => 5,
            Operator::LogicalOr | Operator::LogicalOrDeprecated => 10,
            Operator::LogicalAnd | Operator::LogicalAndDeprecated => 11,
            Operator::BitwiseOr => 12,
            Operator::BitwiseXor => 13,
            Operator::BitwiseAnd => 14,
            Operator::Equality | Operator::Inequality => 20,
            Operator::LessThanOrEqual
            | Operator::LessThan
            | Operator::GreaterThanOrEqual
            | Operator::GreaterThan => 25,
            Operator::RightShift | Operator::LeftShift => 28,
            Operator::Addition | Operator::Subtraction => 30,
            Operator::Multiplication | Operator::Division | Operator::Modulus => 35,
            Operator::Exponentiation => 40,
        }
    }

    /// We parse ternary operators by treating them as binary operators, e.g., a?
    /// b | c, becomes a binary operator with `? b |` as operator symbol.  We
    /// only need to match on `?` to figure out what the operator is, but when we
    /// consume the token, we also consume `b` as an expression and the `|`.  The
    /// inner expression is returned as an optional return value, which can then
    /// be passed to [Self::apply].
    pub fn consume(&self, input: ParseStream) -> Result<Option<Rc<Expression>>> {
        match self {
            Operator::CopyAndUpdate => {
                input.parse::<Ident>()?;
                input.parse::<Token![/]>()?;
                let expr = parse_expression(input, true)?;
                input.parse::<Token![<]>()?;
                input.parse::<Token![-]>()?;
                return Ok(Some(expr));
            }
            Operator::Range => {
                input.parse::<Token![..]>()?;
            }
            Operator::Conditional => {
                input.parse::<Token![?]>()?;
                let expr = parse_expression(input, false)?;
                input.parse::<Token![|]>()?;
                return Ok(Some(expr));
            }
            Operator::LogicalAnd | Operator::LogicalOr => {
                input.parse::<Ident>()?;
            }
            Operator::LogicalOrDeprecated => {
                input.parse::<Token![|]>()?;
                input.parse::<Token![|]>()?;
            }
            Operator::LogicalAndDeprecated => {
                input.parse::<Token![&]>()?;
                input.parse::<Token![&]>()?;
            }
            Operator::BitwiseOr => {
                input.parse::<Token![|]>()?;
                input.parse::<Token![|]>()?;
                input.parse::<Token![|]>()?;
            }
            Operator::BitwiseXor => {
                input.parse::<Token![^]>()?;
                input.parse::<Token![^]>()?;
                input.parse::<Token![^]>()?;
            }
            Operator::BitwiseAnd => {
                input.parse::<Token![&]>()?;
                input.parse::<Token![&]>()?;
                input.parse::<Token![&]>()?;
            }
            Operator::Equality => {
                input.parse::<Token![=]>()?;
                input.parse::<Token![=]>()?;
            }
            Operator::Inequality => {
                input.parse::<Token![!]>()?;
                input.parse::<Token![=]>()?;
            }
            Operator::LessThanOrEqual => {
                input.parse::<Token![<]>()?;
                input.parse::<Token![=]>()?;
            }
            Operator::LessThan => {
                input.parse::<Token![<]>()?;
            }
            Operator::GreaterThanOrEqual => {
                input.parse::<Token![>]>()?;
                input.parse::<Token![=]>()?;
            }
            Operator::GreaterThan => {
                input.parse::<Token![>]>()?;
            }
            Operator::RightShift => {
                input.parse::<Token![>]>()?;
                input.parse::<Token![>]>()?;
                input.parse::<Token![>]>()?;
            }
            Operator::LeftShift => {
                input.parse::<Token![<]>()?;
                input.parse::<Token![<]>()?;
                input.parse::<Token![<]>()?;
            }
            Operator::Addition => {
                input.parse::<Token![+]>()?;
            }
            Operator::Subtraction => {
                input.parse::<Token![-]>()?;
            }
            Operator::Multiplication => {
                input.parse::<Token![*]>()?;
            }
            Operator::Division => {
                input.parse::<Token![/]>()?;
            }
            Operator::Modulus => {
                input.parse::<Token![%]>()?;
            }
            Operator::Exponentiation => {
                input.parse::<Token![^]>()?;
            }
        }

        Ok(None)
    }

    pub fn apply(
        &self,
        lhs: Rc<Expression>,
        rhs: Rc<Expression>,
        extra: Option<Rc<Expression>>,
    ) -> Rc<Expression> {
        match self {
            Operator::CopyAndUpdate => Expression::copy_and_update(lhs, extra.unwrap(), rhs),
            Operator::Range => Expression::range(lhs, rhs),
            Operator::Conditional => Expression::conditional(lhs, extra.unwrap(), rhs),
            Operator::LogicalOr | Operator::LogicalOrDeprecated => Expression::logical_or(lhs, rhs),
            Operator::LogicalAnd | Operator::LogicalAndDeprecated => {
                Expression::logical_and(lhs, rhs)
            }
            Operator::BitwiseOr => Expression::bitwise_or(lhs, rhs),
            Operator::BitwiseXor => Expression::bitwise_xor(lhs, rhs),
            Operator::BitwiseAnd => Expression::bitwise_and(lhs, rhs),
            Operator::Equality => Expression::equality(lhs, rhs),
            Operator::Inequality => Expression::inequality(lhs, rhs),
            Operator::LessThan => Expression::less_than(lhs, rhs),
            Operator::LessThanOrEqual => Expression::less_than_or_equal(lhs, rhs),
            Operator::GreaterThan => Expression::greater_than(lhs, rhs),
            Operator::GreaterThanOrEqual => Expression::greater_than_or_equal(lhs, rhs),
            Operator::RightShift => Expression::right_shift(lhs, rhs),
            Operator::LeftShift => Expression::left_shift(lhs, rhs),
            Operator::Addition => Expression::addition(lhs, rhs),
            Operator::Subtraction => Expression::subtraction(lhs, rhs),
            Operator::Multiplication => Expression::multiplication(lhs, rhs),
            Operator::Division => Expression::division(lhs, rhs),
            Operator::Modulus => Expression::modulus(lhs, rhs),
            Operator::Exponentiation => Expression::exponentiation(lhs, rhs),
        }
    }
}

/// Array index expression (e.g., `idx`, `idx...`)
#[derive(Debug, PartialEq)]
pub enum ArrayItemIndex {
    // [expr]
    Simple(Rc<Expression>),
    // [...]
    Ellipsis,
    // [...expr]
    EllipsesPrefix(Rc<Expression>),
    // [expr...]
    EllipsesPostfix(Rc<Expression>),
    // [...]
    EllipsesInfix(Rc<Expression>),
}

impl ArrayItemIndex {
    pub fn simple(expr: Rc<Expression>) -> Self {
        Self::Simple(expr)
    }

    pub fn ellipsis() -> Self {
        Self::Ellipsis
    }

    pub fn prefix(expr: Rc<Expression>) -> Self {
        Self::EllipsesPrefix(expr)
    }

    pub fn postfix(expr: Rc<Expression>) -> Self {
        Self::EllipsesPostfix(expr)
    }

    pub fn infix(expr: Rc<Expression>) -> Self {
        Self::EllipsesInfix(expr)
    }
}

impl Parse for ArrayItemIndex {
    fn parse(input: ParseStream) -> Result<Self> {
        let pre_ellipsis = peek_and_consume_ellipsis(input)?;

        if pre_ellipsis && input.is_empty() {
            Ok(Self::Ellipsis)
        } else {
            let expr = parse_expression(input, false)?;
            let post_ellipsis = peek_and_consume_ellipsis(input)?;

            match (pre_ellipsis, post_ellipsis) {
                (false, false) => Ok(Self::Simple(expr)),
                (true, false) => Ok(Self::EllipsesPrefix(expr)),
                (false, true) => Ok(Self::EllipsesPostfix(expr)),
                (true, true) => Ok(Self::EllipsesInfix(expr)),
            }
        }
    }
}

/// A literal value to Q#'s `Pauli` type (`PauliI`, `PauliX`, `PauliY`, and `PauliZ`)
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Pauli {
    PauliI,
    PauliX,
    PauliY,
    PauliZ,
}

/// A literal value to Q#'s `Result` type (`Zero` and `One`)
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ResultValue {
    Zero,
    One,
}

/// Any Q# expression (e.g., literals, unary, binary, and ternary expressions)
#[derive(Debug, PartialEq)]
pub enum Expression {
    // Literals
    UnitValue,
    Missing,
    Identifier(QualifiedName, Option<Vec<Rc<TypeKind>>>),
    IntLiteral(i64),
    BigIntLiteral(Integer),
    DoubleLiteral(f64),
    BoolLiteral(bool),
    StringLiteral(String),
    StringInterpolation(String),
    PauliLiteral(Pauli),
    ResultLiteral(ResultValue),
    ArrayLiteral(Vec<Rc<Self>>),
    SizedArrayLiteral(Rc<Self>, Rc<Self>),
    TupleLiteral(Vec<Rc<Self>>),
    NewLiteral(Rc<TypeKind>, Rc<Self>),

    // Prefix
    PosPrefix(Rc<Self>),
    NegPrefix(Rc<Self>),
    LogicalNot(Rc<Self>),
    BitwiseNot(Rc<Self>),
    Adjoint(Rc<Self>),
    Controlled(Rc<Self>),

    // Infix
    Range(Rc<Self>, Rc<Self>),
    LogicalOr(Rc<Self>, Rc<Self>),
    LogicalAnd(Rc<Self>, Rc<Self>),
    BitwiseOr(Rc<Self>, Rc<Self>),
    BitwiseXor(Rc<Self>, Rc<Self>),
    BitwiseAnd(Rc<Self>, Rc<Self>),
    Equality(Rc<Self>, Rc<Self>),
    Inequality(Rc<Self>, Rc<Self>),
    LessThan(Rc<Self>, Rc<Self>),
    LessThanOrEqual(Rc<Self>, Rc<Self>),
    GreaterThan(Rc<Self>, Rc<Self>),
    GreaterThanOrEqual(Rc<Self>, Rc<Self>),
    RightShift(Rc<Self>, Rc<Self>),
    LeftShift(Rc<Self>, Rc<Self>),
    Addition(Rc<Self>, Rc<Self>),
    Subtraction(Rc<Self>, Rc<Self>),
    Multiplication(Rc<Self>, Rc<Self>),
    Division(Rc<Self>, Rc<Self>),
    Modulus(Rc<Self>, Rc<Self>),
    Exponentiation(Rc<Self>, Rc<Self>),

    // Ternary
    CopyAndUpdate(Rc<Self>, Rc<Self>, Rc<Self>),
    Conditional(Rc<Self>, Rc<Self>, Rc<Self>),

    // Postfix
    Call(Rc<Self>, Vec<Rc<Self>>),
    Unwrap(Rc<Self>),
    ArrayItem(Rc<Self>, ArrayItemIndex),
    NamedItem(Rc<Self>, String),
}

macro_rules! constructor {
    ($name: ident, $lhs: ident, $rhs: ident) => {
        paste! {
            pub fn [<$name:snake>]($lhs: Rc<Self>, $rhs: Rc<Self>) -> Rc<Self> {
                Rc::new(Self::$name($lhs, $rhs))
            }
        }
    };
}

impl Expression {
    pub fn unit_value() -> Rc<Self> {
        Rc::new(Self::UnitValue)
    }

    pub fn missing() -> Rc<Self> {
        Rc::new(Self::Missing)
    }

    pub fn simple_identifier(name: &str) -> Rc<Self> {
        Rc::new(Self::Identifier(QualifiedName::simple(name), None))
    }

    pub fn identifier(name: QualifiedName) -> Rc<Self> {
        Rc::new(Self::Identifier(name, None))
    }

    pub fn typed_identifier(name: QualifiedName, type_arguments: Vec<Rc<TypeKind>>) -> Rc<Self> {
        Rc::new(Self::Identifier(name, Some(type_arguments)))
    }

    pub fn int_literal(value: i64) -> Rc<Self> {
        Rc::new(Self::IntLiteral(value))
    }

    pub fn big_int_literal(value: Integer) -> Rc<Self> {
        Rc::new(Self::BigIntLiteral(value))
    }

    pub fn double_literal(value: f64) -> Rc<Self> {
        Rc::new(Self::DoubleLiteral(value))
    }

    pub fn bool_literal(value: bool) -> Rc<Self> {
        Rc::new(Self::BoolLiteral(value))
    }

    pub fn string_literal(value: String) -> Rc<Self> {
        Rc::new(Self::StringLiteral(value))
    }

    pub fn string_interpolation(value: String) -> Rc<Self> {
        Rc::new(Self::StringInterpolation(value))
    }

    pub fn pauli_literal(value: Pauli) -> Rc<Self> {
        Rc::new(Self::PauliLiteral(value))
    }

    pub fn result_literal(value: ResultValue) -> Rc<Self> {
        Rc::new(Self::ResultLiteral(value))
    }

    pub fn array_literal(values: Vec<Rc<Self>>) -> Rc<Self> {
        Rc::new(Self::ArrayLiteral(values))
    }

    pub fn sized_array_literal(expr: Rc<Self>, size: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::SizedArrayLiteral(expr, size))
    }

    pub fn tuple_literal(values: Vec<Rc<Self>>) -> Rc<Self> {
        Rc::new(Self::TupleLiteral(values))
    }

    pub fn new_literal(type_kind: Rc<TypeKind>, size: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::NewLiteral(type_kind, size))
    }

    pub fn pos_prefix(expr: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::PosPrefix(expr))
    }

    pub fn neg_prefix(expr: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::NegPrefix(expr))
    }

    pub fn logical_not(expr: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::LogicalNot(expr))
    }

    pub fn bitwise_not(expr: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::BitwiseNot(expr))
    }

    pub fn adjoint(expr: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::Adjoint(expr))
    }

    pub fn controlled(expr: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::Controlled(expr))
    }

    constructor!(Range, lhs, rhs);
    constructor!(LogicalOr, lhs, rhs);
    constructor!(LogicalAnd, lhs, rhs);
    constructor!(BitwiseOr, lhs, rhs);
    constructor!(BitwiseXor, lhs, rhs);
    constructor!(BitwiseAnd, lhs, rhs);
    constructor!(Equality, lhs, rhs);
    constructor!(Inequality, lhs, rhs);
    constructor!(LessThan, lhs, rhs);
    constructor!(LessThanOrEqual, lhs, rhs);
    constructor!(GreaterThan, lhs, rhs);
    constructor!(GreaterThanOrEqual, lhs, rhs);
    constructor!(RightShift, lhs, rhs);
    constructor!(LeftShift, lhs, rhs);
    constructor!(Addition, lhs, rhs);
    constructor!(Subtraction, lhs, rhs);
    constructor!(Multiplication, lhs, rhs);
    constructor!(Division, lhs, rhs);
    constructor!(Modulus, lhs, rhs);
    constructor!(Exponentiation, lhs, rhs);

    pub fn copy_and_update(target: Rc<Self>, index: Rc<Self>, expr: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::CopyAndUpdate(target, index, expr))
    }

    pub fn conditional(condition: Rc<Self>, then_case: Rc<Self>, else_case: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::Conditional(condition, then_case, else_case))
    }

    pub fn call(caller: Rc<Self>, arguments: Vec<Rc<Self>>) -> Rc<Self> {
        Rc::new(Self::Call(caller, arguments))
    }

    pub fn unwrap(expr: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::Unwrap(expr))
    }

    pub fn array_item(expr: Rc<Self>, index: ArrayItemIndex) -> Rc<Self> {
        Rc::new(Self::ArrayItem(expr, index))
    }

    pub fn named_item(expr: Rc<Self>, name: &str) -> Rc<Self> {
        Rc::new(Self::NamedItem(expr, String::from(name)))
    }
}

macro_rules! check_next {
    ($rest: expr, $char: literal, $op: ident) => {
        if let Some((punct2, _)) = $rest.punct() {
            if punct2.as_char() == $char {
                return Ok(Operator::$op);
            }
        }
    };
    ($rest: expr, $char: literal, $char2: literal, $op: ident) => {
        if let Some((punct2, rest2)) = $rest.punct() {
            if punct2.as_char() == $char {
                if let Some((punct3, _)) = rest2.punct() {
                    if punct3.as_char() == $char2 {
                        return Ok(Operator::$op);
                    }
                }
            }
        }
    };
}

fn peek_operator(input: ParseStream, parse_with_index: bool) -> Result<Operator> {
    let cursor = input.cursor();

    if let Some((ident, rest)) = cursor.ident() {
        match ident.to_string().as_str() {
            "w" => {
                if let Some((punct, _)) = rest.punct() {
                    if punct.as_char() == '/' {
                        return Ok(Operator::CopyAndUpdate);
                    }
                }
            }
            "or" => return Ok(Operator::LogicalOr),
            "and" => return Ok(Operator::LogicalAnd),
            _ => {}
        }
    } else if let Some((punct, rest)) = cursor.punct() {
        match punct.as_char() {
            '.' => {
                if let Some((punct2, rest2)) = rest.punct() {
                    if punct2.as_char() == '.' {
                        if let Some((punct3, _)) = rest2.punct() {
                            if punct3.as_char() != '.' {
                                return Ok(Operator::Range);
                            }
                        } else {
                            return Ok(Operator::Range);
                        }
                    }
                }
            }
            '?' => return Ok(Operator::Conditional),
            '|' => {
                if let Some((punct2, rest2)) = rest.punct() {
                    if punct2.as_char() == '|' {
                        if let Some((punct3, _)) = rest2.punct() {
                            if punct3.as_char() == '|' {
                                return Ok(Operator::BitwiseOr);
                            }
                        }
                        return Ok(Operator::LogicalOrDeprecated);
                    }
                }
            }
            '^' => {
                check_next!(rest, '^', '^', BitwiseXor);
                return Ok(Operator::Exponentiation);
            }
            '&' => {
                if let Some((punct2, rest2)) = rest.punct() {
                    if punct2.as_char() == '&' {
                        if let Some((punct3, _)) = rest2.punct() {
                            if punct3.as_char() == '&' {
                                return Ok(Operator::BitwiseAnd);
                            }
                        }
                        return Ok(Operator::LogicalAndDeprecated);
                    }
                }
            }
            '=' => check_next!(rest, '=', Equality),
            '!' => check_next!(rest, '=', Inequality),
            '<' => {
                if let Some((punct2, rest2)) = rest.punct() {
                    match punct2.as_char() {
                        '=' => return Ok(Operator::LessThanOrEqual),
                        '<' => check_next!(rest2, '<', LeftShift),
                        '-' if parse_with_index => return Err(input.error("unexpected token")),
                        _ => {}
                    }
                }
                return Ok(Operator::LessThan);
            }
            '>' => {
                if let Some((punct2, rest2)) = rest.punct() {
                    match punct2.as_char() {
                        '=' => return Ok(Operator::GreaterThanOrEqual),
                        '>' => check_next!(rest2, '>', RightShift),
                        _ => {}
                    }
                }
                return Ok(Operator::GreaterThan);
            }
            '+' => return Ok(Operator::Addition),
            '-' => return Ok(Operator::Subtraction),
            '*' => return Ok(Operator::Multiplication),
            '/' => return Ok(Operator::Division),
            '%' => return Ok(Operator::Modulus),
            _ => {}
        }
    }

    Err(input.error("unexpected token"))
}

/// we set `parse_call` to false when parsing a primary after `Adjoint` or
/// `Controlled`, because the call operator has a lower precedence; otherwise,
/// it's always true
fn parse_primary(input: ParseStream, parse_call: bool) -> Result<Rc<Expression>> {
    // TODO optimize order or use low-level cursor API for lookahead
    let mut prefix = if input.peek(Paren) {
        let buffer;
        parenthesized!(buffer in input);
        if buffer.is_empty() {
            Ok(Expression::unit_value())
        } else {
            let first_expr = parse_expression(&buffer, false)?;

            if buffer.is_empty() {
                Ok(first_expr)
            } else {
                let mut values = vec![first_expr];
                while !buffer.is_empty() {
                    buffer.parse::<Token![,]>()?;
                    values.push(parse_expression(&buffer, false)?);
                }
                Ok(Expression::tuple_literal(values))
            }
        }
    } else if input.peek(Bracket) {
        // arrays and sized arrays
        let buffer;
        bracketed!(buffer in input);
        let mut values = vec![];
        let mut sized = None;
        if !buffer.is_empty() {
            values.push(parse_expression(&buffer, false)?);
            let mut first = true;
            while !buffer.is_empty() {
                buffer.parse::<Token![,]>()?;
                if first && buffer.peek(kw::size) && buffer.peek2(Token![=]) {
                    buffer.parse::<kw::size>()?;
                    buffer.parse::<Token![=]>()?;
                    values.push(parse_expression(&buffer, false)?);
                    sized = Some(true);

                    if !buffer.is_empty() {
                        sized = Some(false)
                    }
                } else {
                    values.push(parse_expression(&buffer, false)?);
                }
                first = false;
            }
        }
        match sized {
            None => Ok(Expression::array_literal(values)),
            Some(true) => {
                let size = values.pop().unwrap();
                let expr = values.pop().unwrap();
                Ok(Expression::sized_array_literal(expr, size))
            }
            Some(false) => Err(input.error("invalid sized literal")),
        }
    } else if peek_and_consume(input, kw::new)? {
        let type_kind = parse_type(input)?;
        let buffer;
        bracketed!(buffer in input);
        let size = parse_expression(&buffer, false)?;
        Ok(Expression::new_literal(type_kind, size))
    } else if peek_and_consume(input, Token![+])? {
        // + prefix
        Ok(Expression::pos_prefix(parse_primary(input, true)?))
    } else if peek_and_consume(input, Token![-])? {
        // - prefix
        Ok(Expression::neg_prefix(parse_primary(input, true)?))
    } else if input.peek(Token![~]) && input.peek2(Token![~]) && input.peek3(Token![~]) {
        // ~~~ prefix
        input.parse::<Token![~]>()?;
        input.parse::<Token![~]>()?;
        input.parse::<Token![~]>()?;
        Ok(Expression::bitwise_not(parse_primary(input, true)?))
    } else if peek_and_consume(input, kw::not)? || peek_and_consume(input, Token![!])? {
        Ok(Expression::logical_not(parse_primary(input, true)?))
    } else if peek_and_consume(input, kw::Adjoint)? {
        Ok(Expression::adjoint(parse_primary(input, false)?))
    } else if peek_and_consume(input, kw::Controlled)? {
        Ok(Expression::controlled(parse_primary(input, false)?))
    } else if input.peek(Lit) {
        // terminal case: Int, BigInt, and Double literals
        parse_numeric_literal(input)
    } else if peek_and_consume(input, kw::Zero)? {
        Ok(Expression::result_literal(ResultValue::Zero))
    } else if peek_and_consume(input, kw::One)? {
        Ok(Expression::result_literal(ResultValue::One))
    } else if peek_and_consume(input, kw::PauliI)? {
        Ok(Expression::pauli_literal(Pauli::PauliI))
    } else if peek_and_consume(input, kw::PauliX)? {
        Ok(Expression::pauli_literal(Pauli::PauliX))
    } else if peek_and_consume(input, kw::PauliY)? {
        Ok(Expression::pauli_literal(Pauli::PauliY))
    } else if peek_and_consume(input, kw::PauliZ)? {
        Ok(Expression::pauli_literal(Pauli::PauliZ))
    } else if peek_and_consume(input, Token![$])? {
        let lit: LitStr = input.parse()?;
        Ok(Expression::string_interpolation(lit.value()))
    } else if peek_and_consume(input, Token![.])? {
        // TODO ensure that fp value does not contain .
        let lit: Lit = input.parse()?;

        let value: f64 = match lit {
            Lit::Float(lit) => format!("0.{}", lit.base10_digits()),
            Lit::Int(lit) => format!("0.{}", lit.base10_digits()),
            _ => return Err(input.error("cannot parse floating point literal")),
        }
        .parse()
        .map_err(|_| input.error("cannot parse floating point literal"))?;

        Ok(Expression::double_literal(value))
    } else {
        // terminal case: identifier literal with qualified name
        let name: QualifiedName = input.parse()?;

        if name.is_simple_name("_") {
            Ok(Expression::missing())
        } else {
            // TODO can we parse this better without such a large look-ahead (maybe with step)
            let forked = input.fork();
            if let Ok(TypeParameters(Some(_))) = forked.parse() {
                let kinds = input.parse::<TypeParameters>()?.0.unwrap();
                Ok(Expression::typed_identifier(name, kinds))
            } else {
                Ok(Expression::identifier(name))
            }
        }
    }?;

    // array item and named item have same precedence
    loop {
        if input.peek(Bracket) {
            // array item
            let buffer;
            bracketed!(buffer in input);

            //let index = parse_expression(&buffer, false)?;
            let index = buffer.parse()?;
            prefix = Expression::array_item(prefix, index);
            continue;
        } else if input.peek(Token![::]) {
            // named item
            input.parse::<Token![::]>()?;
            let name: Ident = input.parse()?;
            prefix = Expression::named_item(prefix, name.to_string().as_str());
            continue;
        } else if input.peek(Token![!]) && !input.peek2(Token![=]) {
            // unwrap
            input.parse::<Token![!]>()?;
            prefix = Expression::unwrap(prefix);
            continue;
        }
        break;
    }

    // call
    while parse_call && input.peek(Paren) {
        let buffer;
        parenthesized!(buffer in input);

        let arguments = if buffer.is_empty() {
            vec![]
        } else {
            let arguments: Punctuated<Rc<Expression>, Token![,]> =
                Punctuated::parse_separated_nonempty_with(&buffer, |buffer| {
                    parse_expression(buffer, false)
                })?;
            arguments.into_iter().collect()
        };

        prefix = Expression::call(prefix, arguments);
    }

    Ok(prefix)
}

fn parse_expression_1(
    input: ParseStream,
    lhs: Rc<Expression>,
    min_precedence: u32,
    parse_with_index: bool,
) -> Result<Rc<Expression>> {
    let mut lhs = lhs;

    let mut lookahead = peek_operator(input, parse_with_index);

    while let Ok(operator) = lookahead {
        if operator.precedence() < min_precedence {
            break;
        }
        let extra = operator.consume(input)?;
        let mut rhs = parse_primary(input, true)?;

        lookahead = peek_operator(input, parse_with_index);
        while let Ok(operator2) = lookahead {
            if operator2.precedence() <= operator.precedence() {
                break;
            }
            rhs = parse_expression_1(input, rhs, operator.precedence() + 1, parse_with_index)?;
            lookahead = peek_operator(input, parse_with_index);
        }

        lhs = operator.apply(lhs, rhs, extra);
    }

    Ok(lhs)
}

fn parse_numeric_literal(input: ParseStream) -> Result<Rc<Expression>> {
    // TODO string

    let literal: Lit = input.parse()?;

    match literal {
        Lit::Str(lit) => Ok(Expression::string_literal(lit.value())),
        Lit::ByteStr(_) | Lit::Byte(_) | Lit::Char(_) | Lit::Verbatim(_) => {
            Err(input.error("unsupported literal"))
        }
        Lit::Int(lit) => match lit.suffix() {
            "" => Ok(Expression::int_literal(lit.base10_parse::<i64>()?)),
            "l" | "L" => {
                let integer = lit.base10_digits().parse::<Integer>();
                let integer = integer.map_err(|_| input.error("cannot parse BigInt literal"))?;
                Ok(Expression::big_int_literal(integer))
            }
            other => Err(input.error(format!("unsupported suffix {}", other))),
        },
        Lit::Float(lit) => Ok(Expression::double_literal(lit.base10_parse::<f64>()?)),
        Lit::Bool(lit) => Ok(Expression::bool_literal(lit.value())),
    }
}

pub(crate) fn parse_expression(
    input: ParseStream,
    parse_with_index: bool,
) -> Result<Rc<Expression>> {
    let expr = parse_primary(input, true)?;
    parse_expression_1(input, expr, 0, parse_with_index)
}

impl Parse for Expression {
    fn parse(input: ParseStream) -> Result<Self> {
        //let expr = parse_expression(input, false)?;
        let expr = parse_expression(input, false)?;
        Rc::try_unwrap(expr).map_err(|_| input.error("cannot extract expression"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use syn::Result;

    fn parse_expression(s: &str) -> Result<Expression> {
        syn::parse_str(s)
    }

    #[test]
    fn test_identifiers() -> Result<()> {
        assert_eq!(parse_expression("x")?, *Expression::simple_identifier("x"));
        assert_eq!(
            parse_expression("a.b.c")?,
            *Expression::identifier(QualifiedName::new(&["a", "b"], "c"))
        );

        Ok(())
    }

    #[test]
    fn test_literals() -> Result<()> {
        assert_eq!(parse_expression("()")?, *Expression::unit_value());
        assert_eq!(parse_expression("1")?, *Expression::int_literal(1));
        assert_eq!(
            parse_expression("+1")?,
            *Expression::pos_prefix(Expression::int_literal(1))
        );
        assert_eq!(
            parse_expression("-1")?,
            *Expression::neg_prefix(Expression::int_literal(1))
        );
        assert_eq!(parse_expression("0x1")?, *Expression::int_literal(1));
        assert_eq!(
            parse_expression("+0x1")?,
            *Expression::pos_prefix(Expression::int_literal(1))
        );
        assert_eq!(
            parse_expression("1L")?,
            *Expression::big_int_literal(Integer::from(1))
        );
        assert_eq!(
            parse_expression("+1L")?,
            *Expression::pos_prefix(Expression::big_int_literal(Integer::from(1)))
        );
        assert_eq!(
            parse_expression("-1L")?,
            *Expression::neg_prefix(Expression::big_int_literal(Integer::from(1)))
        );
        assert_eq!(
            parse_expression("10000000000000000L")?,
            *Expression::big_int_literal("10000000000000000".parse::<Integer>().unwrap())
        );
        assert_eq!(
            parse_expression("0b111L")?,
            *Expression::big_int_literal(Integer::from(7))
        );
        assert_eq!(
            parse_expression("0b1101L")?,
            *Expression::big_int_literal(Integer::from(13))
        );
        assert_eq!(
            parse_expression("0b1100101011111110L")?,
            *Expression::big_int_literal(Integer::from(51966))
        );
        assert_eq!(
            parse_expression("0o1L")?,
            *Expression::big_int_literal(Integer::from(1))
        );
        assert_eq!(
            parse_expression("0o105L")?,
            *Expression::big_int_literal(Integer::from(69))
        );
        assert_eq!(
            parse_expression("0o12345L")?,
            *Expression::big_int_literal(Integer::from(5349))
        );
        assert_eq!(
            parse_expression("0xfL")?,
            *Expression::big_int_literal(Integer::from(15))
        );
        assert_eq!(
            parse_expression("0xffL")?,
            *Expression::big_int_literal(Integer::from(255))
        );
        assert_eq!(
            parse_expression("1l")?,
            *Expression::big_int_literal(Integer::from(1))
        );
        assert_eq!(
            parse_expression("+1l")?,
            *Expression::pos_prefix(Expression::big_int_literal(Integer::from(1)))
        );
        assert_eq!(
            parse_expression("-1l")?,
            *Expression::neg_prefix(Expression::big_int_literal(Integer::from(1)))
        );
        assert_eq!(
            parse_expression("10000000000000000l")?,
            *Expression::big_int_literal("10000000000000000".parse::<Integer>().unwrap())
        );
        assert_eq!(
            parse_expression("0xfl")?,
            *Expression::big_int_literal(Integer::from(15))
        );
        assert_eq!(
            parse_expression("+0xfl")?,
            *Expression::pos_prefix(Expression::big_int_literal(Integer::from(15)))
        );
        assert_eq!(
            parse_expression("0xffl")?,
            *Expression::big_int_literal(Integer::from(255))
        );
        assert_eq!(
            parse_expression("+0xffl")?,
            *Expression::pos_prefix(Expression::big_int_literal(Integer::from(255)))
        );
        assert_eq!(parse_expression("0o1")?, *Expression::int_literal(1));
        assert_eq!(
            parse_expression("+0o1")?,
            *Expression::pos_prefix(Expression::int_literal(1))
        );
        assert_eq!(
            parse_expression("-0o1")?,
            *Expression::neg_prefix(Expression::int_literal(1))
        );
        assert_eq!(parse_expression("0b1")?, *Expression::int_literal(1));
        assert_eq!(parse_expression("0b100")?, *Expression::int_literal(4));
        assert_eq!(
            parse_expression("+0b100")?,
            *Expression::pos_prefix(Expression::int_literal(4))
        );
        assert_eq!(
            parse_expression("-0b100")?,
            *Expression::neg_prefix(Expression::int_literal(4))
        );
        assert_eq!(parse_expression("0o1")?, *Expression::int_literal(1));
        assert_eq!(parse_expression("0o100")?, *Expression::int_literal(64));
        assert_eq!(
            parse_expression("+0o100")?,
            *Expression::pos_prefix(Expression::int_literal(64))
        );
        assert_eq!(
            parse_expression("-0o100")?,
            *Expression::neg_prefix(Expression::int_literal(64))
        );
        assert_eq!(
            parse_expression(".1e-1")?,
            *Expression::double_literal(0.01)
        );
        assert_eq!(parse_expression(".1")?, *Expression::double_literal(0.1));
        assert_eq!(
            parse_expression(".001e3")?,
            *Expression::double_literal(1.0)
        );
        assert_eq!(parse_expression("1.0")?, *Expression::double_literal(1.0));
        assert_eq!(parse_expression("1.")?, *Expression::double_literal(1.0));
        assert_eq!(
            parse_expression("+1.0")?,
            *Expression::pos_prefix(Expression::double_literal(1.0))
        );
        assert_eq!(
            parse_expression("-1.0")?,
            *Expression::neg_prefix(Expression::double_literal(1.0))
        );
        assert_eq!(
            parse_expression("-1.0e2")?,
            *Expression::neg_prefix(Expression::double_literal(100.0))
        );
        assert_eq!(
            parse_expression("-1.0e-2")?,
            *Expression::neg_prefix(Expression::double_literal(0.01))
        );
        assert_eq!(
            parse_expression("One")?,
            *Expression::result_literal(ResultValue::One)
        );
        assert_eq!(
            parse_expression("Zero")?,
            *Expression::result_literal(ResultValue::Zero)
        );
        assert_eq!(
            parse_expression("PauliI")?,
            *Expression::pauli_literal(Pauli::PauliI)
        );
        assert_eq!(
            parse_expression("PauliX")?,
            *Expression::pauli_literal(Pauli::PauliX)
        );
        assert_eq!(
            parse_expression("PauliY")?,
            *Expression::pauli_literal(Pauli::PauliY)
        );
        assert_eq!(
            parse_expression("PauliZ")?,
            *Expression::pauli_literal(Pauli::PauliZ)
        );
        assert_eq!(parse_expression("true")?, *Expression::bool_literal(true));
        assert_eq!(parse_expression("false")?, *Expression::bool_literal(false));

        assert_eq!(
            parse_expression("\"Hello\"")?,
            *Expression::string_literal("Hello".into())
        );
        assert_eq!(
            parse_expression("$\"Hello\"")?,
            *Expression::string_interpolation("Hello".into())
        );

        Ok(())
    }

    macro_rules! test_binary {
        ($expr: literal, $constructor: ident) => {
            assert_eq!(
                parse_expression($expr)?,
                *Expression::$constructor(
                    Expression::simple_identifier("a"),
                    Expression::simple_identifier("b")
                )
            );
        };
    }

    #[test]
    fn test_binary_expressions() -> Result<()> {
        test_binary!("a .. b", range);
        test_binary!("a or b", logical_or);
        test_binary!("a and b", logical_and);
        test_binary!("a ||| b", bitwise_or);
        test_binary!("a ^^^ b", bitwise_xor);
        test_binary!("a &&& b", bitwise_and);
        test_binary!("a == b", equality);
        test_binary!("a != b", inequality);
        test_binary!("a < b", less_than);
        test_binary!("a <= b", less_than_or_equal);
        test_binary!("a > b", greater_than);
        test_binary!("a >= b", greater_than_or_equal);
        test_binary!("a >>> b", right_shift);
        test_binary!("a <<< b", left_shift);
        test_binary!("a + b", addition);
        test_binary!("a - b", subtraction);
        test_binary!("a * b", multiplication);
        test_binary!("a / b", division);
        test_binary!("a % b", modulus);
        test_binary!("a ^ b", exponentiation);

        Ok(())
    }

    #[test]
    fn test_ternary_expressions() -> Result<()> {
        assert_eq!(
            parse_expression("a ? b | c")?,
            *Expression::conditional(
                Expression::simple_identifier("a"),
                Expression::simple_identifier("b"),
                Expression::simple_identifier("c")
            )
        );
        assert_eq!(
            parse_expression("a w/ b <- c")?,
            *Expression::copy_and_update(
                Expression::simple_identifier("a"),
                Expression::simple_identifier("b"),
                Expression::simple_identifier("c")
            )
        );
        // this is different from array update
        assert_eq!(
            parse_expression("a < -b")?,
            *Expression::less_than(
                Expression::simple_identifier("a"),
                Expression::neg_prefix(Expression::simple_identifier("b"))
            )
        );

        Ok(())
    }

    #[test]
    fn test_precedence() -> Result<()> {
        assert_eq!(
            parse_expression("a + b * c")?,
            *Expression::addition(
                Expression::simple_identifier("a"),
                Expression::multiplication(
                    Expression::simple_identifier("b"),
                    Expression::simple_identifier("c")
                )
            )
        );
        assert_eq!(
            parse_expression("a * b + c")?,
            *Expression::addition(
                Expression::multiplication(
                    Expression::simple_identifier("a"),
                    Expression::simple_identifier("b")
                ),
                Expression::simple_identifier("c")
            )
        );

        Ok(())
    }

    macro_rules! test_simple_arithmetic {
        ($expr: literal, $op: ident) => {
            assert_eq!(
                parse_expression($expr)?,
                *Expression::$op(Expression::int_literal(1), Expression::int_literal(1))
            );
        };
    }

    #[test]
    fn test_simple_arithmetic_expression() -> Result<()> {
        assert_eq!(
            parse_expression("-  1")?,
            *Expression::neg_prefix(Expression::int_literal(1))
        );
        assert_eq!(
            parse_expression("~~~1")?,
            *Expression::bitwise_not(Expression::int_literal(1))
        );
        assert_eq!(
            parse_expression("1L+1L")?,
            *Expression::addition(
                Expression::big_int_literal(Integer::from(1)),
                Expression::big_int_literal(Integer::from(1))
            )
        );
        test_simple_arithmetic!("1+1", addition);
        test_simple_arithmetic!("1-1", subtraction);
        test_simple_arithmetic!("1*1", multiplication);
        test_simple_arithmetic!("1/1", division);
        test_simple_arithmetic!("1%1", modulus);
        test_simple_arithmetic!("1^1", exponentiation);
        test_simple_arithmetic!("1|||1", bitwise_or);
        test_simple_arithmetic!("1&&&1", bitwise_and);
        test_simple_arithmetic!("1^^^1", bitwise_xor);
        test_simple_arithmetic!("1>>>1", right_shift);
        test_simple_arithmetic!("1<<<1", left_shift);

        Ok(())
    }

    macro_rules! test_simple_boolean {
        ($expr: literal, $op: ident) => {
            assert_eq!(
                parse_expression($expr)?,
                *Expression::$op(
                    Expression::bool_literal(true),
                    Expression::bool_literal(true)
                )
            );
        };
    }

    #[test]
    fn test_simple_boolean_expression() -> Result<()> {
        test_simple_boolean!("true && true", logical_and);
        test_simple_boolean!("true || true", logical_or);
        test_simple_boolean!("true and true", logical_and);
        test_simple_boolean!("true or true", logical_or);
        assert_eq!(
            parse_expression("!true")?,
            *Expression::logical_not(Expression::bool_literal(true))
        );
        assert_eq!(
            parse_expression("not true")?,
            *Expression::logical_not(Expression::bool_literal(true))
        );

        Ok(())
    }

    macro_rules! test_simple_comparison {
        ($expr: literal, $op: ident) => {
            assert_eq!(
                parse_expression($expr)?,
                *Expression::$op(Expression::int_literal(1), Expression::int_literal(2))
            )
        };
    }

    #[test]
    fn test_simple_comparison_expression() -> Result<()> {
        test_simple_comparison!("1<2", less_than);
        test_simple_comparison!("1<=2", less_than_or_equal);
        test_simple_comparison!("1>2", greater_than);
        test_simple_comparison!("1>=2", greater_than_or_equal);
        test_simple_comparison!("1==2", equality);
        test_simple_comparison!("1!=2", inequality);
        assert_eq!(
            parse_expression("1<2 or 1>2")?,
            *Expression::logical_or(
                Expression::less_than(Expression::int_literal(1), Expression::int_literal(2)),
                Expression::greater_than(Expression::int_literal(1), Expression::int_literal(2))
            )
        );
        Ok(())
    }

    #[test]
    fn test_complex_literals() -> Result<()> {
        assert_eq!(parse_expression("[]")?, *Expression::array_literal(vec![]));
        assert_eq!(parse_expression("[ ]")?, *Expression::array_literal(vec![]));
        assert_eq!(
            parse_expression("[1,2,3]")?,
            *Expression::array_literal(vec![
                Expression::int_literal(1),
                Expression::int_literal(2),
                Expression::int_literal(3),
            ])
        );
        assert_eq!(
            parse_expression("[1,x,3]")?,
            *Expression::array_literal(vec![
                Expression::int_literal(1),
                Expression::simple_identifier("x"),
                Expression::int_literal(3),
            ])
        );
        assert_eq!(
            parse_expression("[1, size = -1]")?,
            *Expression::sized_array_literal(
                Expression::int_literal(1),
                Expression::neg_prefix(Expression::int_literal(1)),
            )
        );
        assert_eq!(
            parse_expression("[1, size = 0]")?,
            *Expression::sized_array_literal(
                Expression::int_literal(1),
                Expression::int_literal(0),
            )
        );
        assert_eq!(
            parse_expression("[1, size = 3]")?,
            *Expression::sized_array_literal(
                Expression::int_literal(1),
                Expression::int_literal(3),
            )
        );
        assert_eq!(
            parse_expression("[1, size = n]")?,
            *Expression::sized_array_literal(
                Expression::int_literal(1),
                Expression::simple_identifier("n"),
            )
        );
        assert_eq!(
            parse_expression("[x, size = n]")?,
            *Expression::sized_array_literal(
                Expression::simple_identifier("x"),
                Expression::simple_identifier("n"),
            )
        );
        assert_eq!(
            parse_expression("[x, size=n]")?,
            *Expression::sized_array_literal(
                Expression::simple_identifier("x"),
                Expression::simple_identifier("n"),
            )
        );
        assert_eq!(
            parse_expression("[[x], size=n]")?,
            *Expression::sized_array_literal(
                Expression::array_literal(vec![Expression::simple_identifier("x")]),
                Expression::simple_identifier("n"),
            )
        );
        // TODO "[\"foo\", size = n + 1]"
        assert_eq!(
            parse_expression("(1,2,3)")?,
            *Expression::tuple_literal(vec![
                Expression::int_literal(1),
                Expression::int_literal(2),
                Expression::int_literal(3),
            ])
        );
        assert_eq!(
            parse_expression("(x,2,3)")?,
            *Expression::tuple_literal(vec![
                Expression::simple_identifier("x"),
                Expression::int_literal(2),
                Expression::int_literal(3),
            ])
        );
        assert_eq!(
            parse_expression("1..2")?,
            *Expression::range(Expression::int_literal(1), Expression::int_literal(2))
        );
        assert_eq!(
            parse_expression("1..2..3")?,
            *Expression::range(
                Expression::range(Expression::int_literal(1), Expression::int_literal(2)),
                Expression::int_literal(3)
            )
        );
        assert_eq!(
            parse_expression("1..2..x")?,
            *Expression::range(
                Expression::range(Expression::int_literal(1), Expression::int_literal(2)),
                Expression::simple_identifier("x")
            )
        );
        assert_eq!(
            parse_expression("1..x..3")?,
            *Expression::range(
                Expression::range(
                    Expression::int_literal(1),
                    Expression::simple_identifier("x")
                ),
                Expression::int_literal(3)
            )
        );
        assert_eq!(
            parse_expression("x..2..3")?,
            *Expression::range(
                Expression::range(
                    Expression::simple_identifier("x"),
                    Expression::int_literal(2)
                ),
                Expression::int_literal(3)
            )
        );
        assert_eq!(
            parse_expression("1..x")?,
            *Expression::range(
                Expression::int_literal(1),
                Expression::simple_identifier("x")
            )
        );
        assert_eq!(
            parse_expression("x..2")?,
            *Expression::range(
                Expression::simple_identifier("x"),
                Expression::int_literal(2)
            )
        );
        assert_eq!(
            parse_expression("x..y")?,
            *Expression::range(
                Expression::simple_identifier("x"),
                Expression::simple_identifier("y")
            )
        );
        Ok(())
    }

    #[test]
    fn test_calls() -> Result<()> {
        assert_eq!(
            parse_expression("Op1()")?,
            *Expression::call(Expression::simple_identifier("Op1"), vec![])
        );

        assert_eq!(
            parse_expression("H(x)")?,
            *Expression::call(
                Expression::simple_identifier("H"),
                vec![Expression::simple_identifier("x")]
            )
        );

        assert_eq!(
            parse_expression("H(x, y)")?,
            *Expression::call(
                Expression::simple_identifier("H"),
                vec![
                    Expression::simple_identifier("x"),
                    Expression::simple_identifier("y")
                ]
            )
        );

        assert_eq!(
            parse_expression("H(x, Add(y, z))")?,
            *Expression::call(
                Expression::simple_identifier("H"),
                vec![
                    Expression::simple_identifier("x"),
                    Expression::call(
                        Expression::simple_identifier("Add"),
                        vec![
                            Expression::simple_identifier("y"),
                            Expression::simple_identifier("z")
                        ]
                    )
                ]
            )
        );

        assert_eq!(
            parse_expression("x()")?,
            *Expression::call(Expression::simple_identifier("x"), vec![])
        );
        assert_eq!(
            parse_expression("x(1,2)")?,
            *Expression::call(
                Expression::simple_identifier("x"),
                vec![Expression::int_literal(1), Expression::int_literal(2)]
            )
        );
        assert_eq!(
            parse_expression("Adjoint x()")?,
            *Expression::call(
                Expression::adjoint(Expression::simple_identifier("x")),
                vec![]
            )
        );
        assert_eq!(
            parse_expression("Controlled x()")?,
            *Expression::call(
                Expression::controlled(Expression::simple_identifier("x")),
                vec![]
            )
        );
        assert_eq!(
            parse_expression("f(1)(2)")?,
            *Expression::call(
                Expression::call(
                    Expression::simple_identifier("f"),
                    vec![Expression::int_literal(1)]
                ),
                vec![Expression::int_literal(2)]
            )
        );
        assert_eq!(
            parse_expression("f(1)(2)(3)")?,
            *Expression::call(
                Expression::call(
                    Expression::call(
                        Expression::simple_identifier("f"),
                        vec![Expression::int_literal(1)]
                    ),
                    vec![Expression::int_literal(2)]
                ),
                vec![Expression::int_literal(3)]
            )
        );
        assert!(parse_expression("f(1)(2)[3]").is_err());
        assert_eq!(
            parse_expression("(f(1)(2))[3]")?,
            *Expression::array_item(
                Expression::call(
                    Expression::call(
                        Expression::simple_identifier("f"),
                        vec![Expression::int_literal(1)],
                    ),
                    vec![Expression::int_literal(2)],
                ),
                ArrayItemIndex::simple(Expression::int_literal(3))
            )
        );
        assert_eq!(
            parse_expression("(f(1)(2))[3](4)")?,
            *Expression::call(
                Expression::array_item(
                    Expression::call(
                        Expression::call(
                            Expression::simple_identifier("f"),
                            vec![Expression::int_literal(1)],
                        ),
                        vec![Expression::int_literal(2)],
                    ),
                    ArrayItemIndex::simple(Expression::int_literal(3))
                ),
                vec![Expression::int_literal(4)]
            )
        );
        assert!(parse_expression("f(1)(2)::X").is_err());
        assert_eq!(
            parse_expression("(f(1)(2))::X")?,
            *Expression::named_item(
                Expression::call(
                    Expression::call(
                        Expression::simple_identifier("f"),
                        vec![Expression::int_literal(1)],
                    ),
                    vec![Expression::int_literal(2)],
                ),
                "X"
            )
        );
        assert_eq!(
            parse_expression("(f(1)(2))::X(4)")?,
            *Expression::call(
                Expression::named_item(
                    Expression::call(
                        Expression::call(
                            Expression::simple_identifier("f"),
                            vec![Expression::int_literal(1)],
                        ),
                        vec![Expression::int_literal(2)],
                    ),
                    "X"
                ),
                vec![Expression::int_literal(4)]
            )
        );
        assert_eq!(
            parse_expression("(x(_,1))(2)")?,
            *Expression::call(
                Expression::call(
                    Expression::simple_identifier("x"),
                    vec![Expression::missing(), Expression::int_literal(1)]
                ),
                vec![Expression::int_literal(2)]
            )
        );
        assert_eq!(
            parse_expression("x(_,1)(2)")?,
            *Expression::call(
                Expression::call(
                    Expression::simple_identifier("x"),
                    vec![Expression::missing(), Expression::int_literal(1)]
                ),
                vec![Expression::int_literal(2)]
            )
        );
        assert_eq!(
            parse_expression("(x(_,1))(1,2)")?,
            *Expression::call(
                Expression::call(
                    Expression::simple_identifier("x"),
                    vec![Expression::missing(), Expression::int_literal(1)]
                ),
                vec![Expression::int_literal(1), Expression::int_literal(2)]
            )
        );
        assert_eq!(
            parse_expression("x(_,1)(1,2)")?,
            *Expression::call(
                Expression::call(
                    Expression::simple_identifier("x"),
                    vec![Expression::missing(), Expression::int_literal(1)]
                ),
                vec![Expression::int_literal(1), Expression::int_literal(2)]
            )
        );
        assert_eq!(
            parse_expression("(x(1,(2, _)))(2)")?,
            *Expression::call(
                Expression::call(
                    Expression::simple_identifier("x"),
                    vec![
                        Expression::int_literal(1),
                        Expression::tuple_literal(vec![
                            Expression::int_literal(2),
                            Expression::missing()
                        ])
                    ]
                ),
                vec![Expression::int_literal(2)]
            )
        );
        assert_eq!(
            parse_expression("x(1,(2, _))(2)")?,
            *Expression::call(
                Expression::call(
                    Expression::simple_identifier("x"),
                    vec![
                        Expression::int_literal(1),
                        Expression::tuple_literal(vec![
                            Expression::int_literal(2),
                            Expression::missing()
                        ])
                    ]
                ),
                vec![Expression::int_literal(2)]
            )
        );
        assert_eq!(
            parse_expression("(x(_,(2, _)))(1, 2)")?,
            *Expression::call(
                Expression::call(
                    Expression::simple_identifier("x"),
                    vec![
                        Expression::missing(),
                        Expression::tuple_literal(vec![
                            Expression::int_literal(2),
                            Expression::missing()
                        ])
                    ]
                ),
                vec![Expression::int_literal(1), Expression::int_literal(2)]
            )
        );
        assert_eq!(
            parse_expression("x(_,(2, _))(1, 2)")?,
            *Expression::call(
                Expression::call(
                    Expression::simple_identifier("x"),
                    vec![
                        Expression::missing(),
                        Expression::tuple_literal(vec![
                            Expression::int_literal(2),
                            Expression::missing()
                        ])
                    ]
                ),
                vec![Expression::int_literal(1), Expression::int_literal(2)]
            )
        );

        Ok(())
    }

    #[test]
    fn test_modifiers() -> Result<()> {
        assert_eq!(
            parse_expression("ab!")?,
            *Expression::unwrap(Expression::simple_identifier("ab"))
        );
        assert_eq!(
            parse_expression("!ab!")?,
            *Expression::logical_not(Expression::unwrap(Expression::simple_identifier("ab")))
        );
        assert_eq!(
            parse_expression("ab!!")?,
            *Expression::unwrap(Expression::unwrap(Expression::simple_identifier("ab")))
        );
        assert_eq!(
            parse_expression("Adjoint x")?,
            *Expression::adjoint(Expression::simple_identifier("x"))
        );
        assert_eq!(
            parse_expression("Controlled Adjoint x")?,
            *Expression::controlled(Expression::adjoint(Expression::simple_identifier("x")))
        );
        assert_eq!(
            parse_expression("ab! ()")?,
            *Expression::call(
                Expression::unwrap(Expression::simple_identifier("ab")),
                vec![]
            )
        );
        assert_eq!(
            parse_expression("ab!! ()")?,
            *Expression::call(
                Expression::unwrap(Expression::unwrap(Expression::simple_identifier("ab"))),
                vec![]
            )
        );
        assert_eq!(
            parse_expression("Adjoint x()")?,
            *Expression::call(
                Expression::adjoint(Expression::simple_identifier("x")),
                vec![]
            )
        );
        assert_eq!(
            parse_expression("Adjoint Controlled x()")?,
            *Expression::call(
                Expression::adjoint(Expression::controlled(Expression::simple_identifier("x"))),
                vec![]
            )
        );
        assert_eq!(
            parse_expression("Adjoint x!()")?,
            *Expression::call(
                Expression::adjoint(Expression::unwrap(Expression::simple_identifier("x"))),
                vec![]
            )
        );
        assert_eq!(
            parse_expression("(udt(x))!")?,
            *Expression::unwrap(Expression::call(
                Expression::simple_identifier("udt"),
                vec![Expression::simple_identifier("x")]
            ))
        );
        assert_eq!(
            parse_expression("(udt(x))! ()")?,
            *Expression::call(
                Expression::unwrap(Expression::call(
                    Expression::simple_identifier("udt"),
                    vec![Expression::simple_identifier("x")]
                )),
                vec![]
            )
        );
        assert_eq!(
            parse_expression("Controlled (udt(x))! ()")?,
            *Expression::call(
                Expression::controlled(Expression::unwrap(Expression::call(
                    Expression::simple_identifier("udt"),
                    vec![Expression::simple_identifier("x")]
                ))),
                vec![]
            )
        );
        assert_eq!(
            parse_expression("(Controlled (udt(x))!) ()")?,
            *Expression::call(
                Expression::controlled(Expression::unwrap(Expression::call(
                    Expression::simple_identifier("udt"),
                    vec![Expression::simple_identifier("x")]
                ))),
                vec![]
            )
        );
        assert_eq!(
            parse_expression("x[i]!")?,
            *Expression::unwrap(Expression::array_item(
                Expression::simple_identifier("x"),
                ArrayItemIndex::simple(Expression::simple_identifier("i"))
            ))
        );
        assert_eq!(
            parse_expression("Adjoint x[i]")?,
            *Expression::adjoint(Expression::array_item(
                Expression::simple_identifier("x"),
                ArrayItemIndex::simple(Expression::simple_identifier("i"))
            ))
        );
        assert_eq!(
            parse_expression("x[i]! ()")?,
            *Expression::call(
                Expression::unwrap(Expression::array_item(
                    Expression::simple_identifier("x"),
                    ArrayItemIndex::simple(Expression::simple_identifier("i"))
                )),
                vec![]
            )
        );
        assert_eq!(
            parse_expression("Adjoint x[i] ()")?,
            *Expression::call(
                Expression::adjoint(Expression::array_item(
                    Expression::simple_identifier("x"),
                    ArrayItemIndex::simple(Expression::simple_identifier("i"))
                )),
                vec![]
            )
        );
        assert_eq!(
            parse_expression("Controlled x[i]! ()")?,
            *Expression::call(
                Expression::controlled(Expression::unwrap(Expression::array_item(
                    Expression::simple_identifier("x"),
                    ArrayItemIndex::simple(Expression::simple_identifier("i"))
                ))),
                vec![]
            )
        );
        assert_eq!(
            parse_expression("x[i]::Re")?,
            *Expression::named_item(
                Expression::array_item(
                    Expression::simple_identifier("x"),
                    ArrayItemIndex::simple(Expression::simple_identifier("i"))
                ),
                "Re"
            )
        );
        assert_eq!(
            parse_expression("x[i]!::Re")?,
            *Expression::named_item(
                Expression::unwrap(Expression::array_item(
                    Expression::simple_identifier("x"),
                    ArrayItemIndex::simple(Expression::simple_identifier("i"))
                )),
                "Re"
            )
        );
        assert_eq!(
            parse_expression("x[i]::Re!")?,
            *Expression::unwrap(Expression::named_item(
                Expression::array_item(
                    Expression::simple_identifier("x"),
                    ArrayItemIndex::simple(Expression::simple_identifier("i"))
                ),
                "Re"
            ))
        );
        assert_eq!(
            parse_expression("x[i]![j]")?,
            *Expression::array_item(
                Expression::unwrap(Expression::array_item(
                    Expression::simple_identifier("x"),
                    ArrayItemIndex::simple(Expression::simple_identifier("i"))
                )),
                ArrayItemIndex::simple(Expression::simple_identifier("j"))
            )
        );
        assert_eq!(
            parse_expression("x::Re![j]")?,
            *Expression::array_item(
                Expression::unwrap(Expression::named_item(
                    Expression::simple_identifier("x"),
                    "Re"
                )),
                ArrayItemIndex::simple(Expression::simple_identifier("j"))
            )
        );
        assert_eq!(
            parse_expression("x::Re!::Im")?,
            *Expression::named_item(
                Expression::unwrap(Expression::named_item(
                    Expression::simple_identifier("x"),
                    "Re"
                )),
                "Im"
            )
        );
        assert_eq!(
            parse_expression("x::Re!!::Im")?,
            *Expression::named_item(
                Expression::unwrap(Expression::unwrap(Expression::named_item(
                    Expression::simple_identifier("x"),
                    "Re"
                ))),
                "Im"
            )
        );
        Ok(())
    }

    #[test]
    fn test_ellipsis() -> Result<()> {
        assert_eq!(
            parse_expression("a[...]")?,
            *Expression::array_item(
                Expression::simple_identifier("a"),
                ArrayItemIndex::ellipsis()
            )
        );
        assert_eq!(
            parse_expression("a[...2]")?,
            *Expression::array_item(
                Expression::simple_identifier("a"),
                ArrayItemIndex::prefix(Expression::int_literal(2))
            )
        );
        assert_eq!(
            parse_expression("a[2...]")?,
            *Expression::array_item(
                Expression::simple_identifier("a"),
                ArrayItemIndex::postfix(Expression::int_literal(2))
            )
        );
        assert_eq!(
            parse_expression("a[...2...]")?,
            *Expression::array_item(
                Expression::simple_identifier("a"),
                ArrayItemIndex::infix(Expression::int_literal(2))
            )
        );
        assert_eq!(
            parse_expression("a[2]")?,
            *Expression::array_item(
                Expression::simple_identifier("a"),
                ArrayItemIndex::simple(Expression::int_literal(2))
            )
        );

        Ok(())
    }

    #[test]
    fn test_type_parameters() -> Result<()> {
        parse_expression("Fact(a < b, 2)")?;
        parse_expression("Fact<a, b>")?;

        Ok(())
    }
}
