use std::rc::Rc;

use proc_macro2::TokenTree;
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    Result, Token,
};

use crate::ast::{
    kw,
    qubit_initializer::parse_qubit_initializer,
    symbol_binding::parse_symbol_binding,
    utilities::{expect_ident, has_extra_parens, peek_and_consume},
    Expression, Ident, QubitInitializer, Scope, SymbolBinding,
};

use super::expression::parse_expression;

/// Update operator (e.g., `+=`, `-=`, `&&&=`)
#[derive(Debug, PartialEq, Clone)]
pub enum UpdateOperator {
    Exponentiation,
    Multiplication,
    Division,
    Modulus,
    Addition,
    Subtraction,
    RightShift,
    LeftShift,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    LogicalAnd,
    LogicalOr,
    ArrayUpdate(Rc<Expression>),
}

impl Parse for UpdateOperator {
    fn parse(input: ParseStream) -> Result<Self> {
        // TODO change to low-level cursor API for performance
        if peek_and_consume(input, Token![^])? {
            if peek_and_consume(input, Token![^])? {
                input.parse::<Token![^]>()?;
                input.parse::<Token![=]>()?;
                Ok(UpdateOperator::BitwiseXor)
            } else {
                input.parse::<Token![=]>()?;
                Ok(UpdateOperator::Exponentiation)
            }
        } else if peek_and_consume(input, Token![*])? {
            input.parse::<Token![=]>()?;
            Ok(UpdateOperator::Multiplication)
        } else if peek_and_consume(input, Token![/])? {
            input.parse::<Token![=]>()?;
            Ok(UpdateOperator::Division)
        } else if peek_and_consume(input, Token![%])? {
            input.parse::<Token![=]>()?;
            Ok(UpdateOperator::Modulus)
        } else if peek_and_consume(input, Token![+])? {
            input.parse::<Token![=]>()?;
            Ok(UpdateOperator::Addition)
        } else if peek_and_consume(input, Token![-])? {
            input.parse::<Token![=]>()?;
            Ok(UpdateOperator::Subtraction)
        } else if peek_and_consume(input, Token![>])? {
            input.parse::<Token![>]>()?;
            input.parse::<Token![>]>()?;
            input.parse::<Token![=]>()?;
            Ok(UpdateOperator::RightShift)
        } else if peek_and_consume(input, Token![<])? {
            input.parse::<Token![<]>()?;
            input.parse::<Token![<]>()?;
            input.parse::<Token![=]>()?;
            Ok(UpdateOperator::LeftShift)
        } else if peek_and_consume(input, Token![&])? {
            input.parse::<Token![&]>()?;
            input.parse::<Token![&]>()?;
            input.parse::<Token![=]>()?;
            Ok(UpdateOperator::BitwiseAnd)
        } else if peek_and_consume(input, Token![|])? {
            input.parse::<Token![|]>()?;
            input.parse::<Token![|]>()?;
            input.parse::<Token![=]>()?;
            Ok(UpdateOperator::BitwiseOr)
        } else if peek_and_consume(input, kw::and)? {
            input.parse::<Token![=]>()?;
            Ok(UpdateOperator::LogicalAnd)
        } else if peek_and_consume(input, kw::or)? {
            input.parse::<Token![=]>()?;
            Ok(UpdateOperator::LogicalOr)
        } else {
            expect_ident(input.parse()?, "w")?;
            input.parse::<Token![/]>()?;
            input.parse::<Token![=]>()?;
            let expr = parse_expression(input, true)?;
            input.parse::<Token![<]>()?;
            input.parse::<Token![-]>()?;
            Ok(UpdateOperator::ArrayUpdate(expr))
        }
    }
}

/// Type of qubit allocation (`use` or `borrow`)
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum QubitAllocationKind {
    Use,
    Borrow,
}

/// Any Q# statement (`let`, `set`, `if`, `for`, ...)
#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Expression(Rc<Expression>),
    Return(Rc<Expression>),
    Fail(Rc<Expression>),
    Let(Rc<SymbolBinding>, Rc<Expression>),
    Set(Rc<SymbolBinding>, Rc<Expression>),
    Update(String, UpdateOperator, Rc<Expression>),
    Mutable(Rc<SymbolBinding>, Rc<Expression>),
    QubitAllocation(
        QubitAllocationKind,
        Rc<SymbolBinding>,
        Rc<QubitInitializer>,
        Option<Scope>,
    ),
    Conditional(Vec<(Rc<Expression>, Scope)>, Option<Scope>),
    For(Rc<SymbolBinding>, Rc<Expression>, Scope),
    While(Rc<Expression>, Scope),
    RepeatUntil(Scope, Rc<Expression>, Option<Scope>),
    WithinApply(Scope, Scope),
}

impl Statement {
    pub fn expression(expression: Rc<Expression>) -> Self {
        Self::Expression(expression)
    }

    pub fn conditional(conditions: Vec<(Rc<Expression>, Scope)>, default: Option<Scope>) -> Self {
        Self::Conditional(conditions, default)
    }

    pub fn within_apply(within_scope: Scope, apply_scope: Scope) -> Self {
        Self::WithinApply(within_scope, apply_scope)
    }
}

impl Parse for Statement {
    fn parse(input: ParseStream) -> Result<Self> {
        // TODO check all keyword-based statements
        let stmt = if peek_and_consume(input, Token![return])? {
            // return ...
            let expr = parse_expression(input, false)?;
            input.parse::<Token![;]>()?;
            Statement::Return(expr)
        } else if peek_and_consume(input, kw::fail)? {
            // fail ...
            let expr = parse_expression(input, false)?;
            input.parse::<Token![;]>()?;
            Statement::Fail(expr)
        } else if peek_and_consume(input, Token![let])? {
            // let ...
            let binding = parse_symbol_binding(input)?;
            input.parse::<Token![=]>()?;
            let expr = parse_expression(input, false)?;
            input.parse::<Token![;]>()?;
            Statement::Let(binding, expr)
        } else if peek_and_consume(input, kw::mutable)? {
            // let ...
            let binding = parse_symbol_binding(input)?;
            input.parse::<Token![=]>()?;
            let expr = parse_expression(input, false)?;
            input.parse::<Token![;]>()?;
            Statement::Mutable(binding, expr)
        } else if peek_and_consume(input, kw::set)? {
            if !input.peek(syn::Ident) || input.peek2(Token![=]) {
                // set ... = ...
                let binding = parse_symbol_binding(input)?;
                input.parse::<Token![=]>()?;
                let expr = parse_expression(input, false)?;
                input.parse::<Token![;]>()?;
                Statement::Set(binding, expr)
            } else {
                // set ... ...= ... (update)
                let ident: Ident = input.parse()?;
                let update_operator = input.parse()?;
                let expr = parse_expression(input, false)?;
                input.parse::<Token![;]>()?;
                Statement::Update(ident.to_string(), update_operator, expr)
            }
        } else if peek_and_consume(input, Token![use])? || peek_and_consume(input, kw::using)? {
            // use | using ...
            // handle optional parentheses; first check whether
            // parentheses exists (one must differentiate between `(a, b) = A`
            // and `((a, b) = A)`).
            let extra_parens = has_extra_parens(input, |tree| {
                if let TokenTree::Punct(punct) = tree {
                    punct.as_char() == '='
                } else {
                    false
                }
            });

            // handle optional parentheses
            let inner;
            let buffer = if extra_parens {
                parenthesized!(inner in input);
                &inner
            } else {
                input
            };

            let symbol_binding = parse_symbol_binding(buffer)?;
            buffer.parse::<Token![=]>()?;
            let qubit_initializer = parse_qubit_initializer(buffer)?;

            if input.peek(Token![;]) {
                input.parse::<Token![;]>()?;
                Statement::QubitAllocation(
                    QubitAllocationKind::Use,
                    symbol_binding,
                    qubit_initializer,
                    None,
                )
            } else {
                let scope = input.parse()?;
                Statement::QubitAllocation(
                    QubitAllocationKind::Use,
                    symbol_binding,
                    qubit_initializer,
                    Some(scope),
                )
            }
        } else if peek_and_consume(input, kw::borrow)? || peek_and_consume(input, kw::borrowing)? {
            // borrow | borrowing ...
            // handle optional parentheses; first check whether
            // parentheses exists (one must differentiate between `(a, b) = A`
            // and `((a, b) = A)`).
            let extra_parens = has_extra_parens(input, |tree| {
                if let TokenTree::Punct(punct) = tree {
                    punct.as_char() == '='
                } else {
                    false
                }
            });

            // handle optional parentheses
            let inner;
            let buffer = if extra_parens {
                parenthesized!(inner in input);
                &inner
            } else {
                input
            };

            let symbol_binding = parse_symbol_binding(buffer)?;
            buffer.parse::<Token![=]>()?;
            let qubit_initializer = parse_qubit_initializer(buffer)?;

            if input.peek(Token![;]) {
                input.parse::<Token![;]>()?;
                Statement::QubitAllocation(
                    QubitAllocationKind::Borrow,
                    symbol_binding,
                    qubit_initializer,
                    None,
                )
            } else {
                let scope = input.parse()?;
                Statement::QubitAllocation(
                    QubitAllocationKind::Borrow,
                    symbol_binding,
                    qubit_initializer,
                    Some(scope),
                )
            }
        } else if peek_and_consume(input, Token![if])? {
            // if ... elif ... else ...
            let condition = parse_expression(input, false)?;
            let scope = input.parse()?;
            let mut pairs = vec![(condition, scope)];

            while peek_and_consume(input, kw::elif)? {
                let condition = parse_expression(input, false)?;
                let scope = input.parse()?;
                pairs.push((condition, scope));
            }

            let default = if peek_and_consume(input, Token![else])? {
                Some(input.parse()?)
            } else {
                None
            };

            Statement::Conditional(pairs, default)
        } else if peek_and_consume(input, Token![for])? {
            // for ... handle optional parentheses; first check whether
            // parentheses exists (one must differentiate between `(a, b) in A`
            // and `((a, b) in A)`).
            let extra_parens = has_extra_parens(input, |tree| {
                if let TokenTree::Ident(ident) = tree {
                    ident == "in"
                } else {
                    false
                }
            });

            let inner;
            let buffer = if extra_parens {
                parenthesized!(inner in input);
                &inner
            } else {
                input
            };

            let binding = parse_symbol_binding(buffer)?;
            buffer.parse::<Token![in]>()?;
            let expr = parse_expression(buffer, false)?;
            let scope = input.parse()?;
            Statement::For(binding, expr, scope)
        } else if peek_and_consume(input, Token![while])? {
            let expr = parse_expression(input, false)?;
            let scope = input.parse()?;
            Statement::While(expr, scope)
        } else if peek_and_consume(input, kw::repeat)? {
            let repeat_scope = input.parse()?;
            input.parse::<kw::until>()?;
            let expr = parse_expression(input, false)?;
            let fixup_scope = if peek_and_consume(input, kw::fixup)? {
                Some(input.parse()?)
            } else {
                input.parse::<Token![;]>()?;
                None
            };
            Statement::RepeatUntil(repeat_scope, expr, fixup_scope)
        } else if peek_and_consume(input, kw::within)? {
            let within_scope = input.parse()?;
            input.parse::<kw::apply>()?;
            let apply_scope = input.parse()?;
            Statement::WithinApply(within_scope, apply_scope)
        } else {
            // if no keyword matches, this must be an expression statement
            let expr = parse_expression(input, false)?;
            input.parse::<Token![;]>()?;
            Statement::Expression(expr)
        };

        Ok(stmt)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use syn::Result;

    fn parse_statement(str: &str) -> Result<Statement> {
        syn::parse_str(str)
    }

    #[test]
    fn test_set_statement() -> Result<()> {
        assert_eq!(
            parse_statement("set a = 5;")?,
            Statement::Set(SymbolBinding::identifier("a"), Expression::int_literal(5))
        );
        assert_eq!(
            parse_statement("set (a, b, _) = (3, 4, false);")?,
            Statement::Set(
                SymbolBinding::tuple(vec![
                    SymbolBinding::identifier("a"),
                    SymbolBinding::identifier("b"),
                    SymbolBinding::discard(),
                ]),
                Expression::tuple_literal(vec![
                    Expression::int_literal(3),
                    Expression::int_literal(4),
                    Expression::bool_literal(false)
                ])
            )
        );

        Ok(())
    }

    macro_rules! test_update_statement {
        ($expr: literal, $op: ident) => {
            assert_eq!(
                parse_statement($expr)?,
                Statement::Update(
                    "a".into(),
                    UpdateOperator::$op,
                    Expression::simple_identifier("b")
                )
            );
        };
    }

    #[test]
    fn test_update_statement() -> Result<()> {
        test_update_statement!("set a ^= b;", Exponentiation);
        test_update_statement!("set a *= b;", Multiplication);
        test_update_statement!("set a /= b;", Division);
        test_update_statement!("set a %= b;", Modulus);
        test_update_statement!("set a += b;", Addition);
        test_update_statement!("set a -= b;", Subtraction);
        test_update_statement!("set a >>>= b;", RightShift);
        test_update_statement!("set a <<<= b;", LeftShift);
        test_update_statement!("set a &&&= b;", BitwiseAnd);
        test_update_statement!("set a ^^^= b;", BitwiseXor);
        test_update_statement!("set a |||= b;", BitwiseOr);
        test_update_statement!("set a and= b;", LogicalAnd);
        test_update_statement!("set a or= b;", LogicalOr);

        assert_eq!(
            parse_statement("set a w/= b <- c;")?,
            Statement::Update(
                "a".into(),
                UpdateOperator::ArrayUpdate(Expression::simple_identifier("b")),
                Expression::simple_identifier("c")
            )
        );

        Ok(())
    }

    #[test]
    fn test_for_statement() -> Result<()> {
        assert_eq!(
            parse_statement("for a in A {}")?,
            Statement::For(
                SymbolBinding::identifier("a"),
                Expression::simple_identifier("A"),
                Scope::new(),
            )
        );
        assert_eq!(
            parse_statement("for (a in A) {}")?,
            Statement::For(
                SymbolBinding::identifier("a"),
                Expression::simple_identifier("A"),
                Scope::new(),
            )
        );

        assert_eq!(
            parse_statement("for (a, b) in A {}")?,
            Statement::For(
                SymbolBinding::tuple(vec![
                    SymbolBinding::identifier("a"),
                    SymbolBinding::identifier("b")
                ]),
                Expression::simple_identifier("A"),
                Scope::new(),
            )
        );
        assert_eq!(
            parse_statement("for ((a, b) in A) {}")?,
            Statement::For(
                SymbolBinding::tuple(vec![
                    SymbolBinding::identifier("a"),
                    SymbolBinding::identifier("b")
                ]),
                Expression::simple_identifier("A"),
                Scope::new(),
            )
        );

        Ok(())
    }
}
