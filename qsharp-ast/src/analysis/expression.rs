use std::{collections::HashMap, rc::Rc};

use crate::ast::{Characteristics, Expression, QualifiedName, TypeKind, TypeTupleItem};

/// Checks whether `actual` is the same as `expected`, if not `None`, otherwise,
/// returns `None`.
fn with_type_check(actual: Option<Rc<TypeKind>>, expected: &Rc<TypeKind>) -> Option<Rc<TypeKind>> {
    actual.filter(|kind| expected == kind)
}

/// Checks whether `actual` is any of `expected`, if not `None`, otherwise,
/// returns `None`.
fn with_type_check_in(
    actual: Option<Rc<TypeKind>>,
    expected: &[Rc<TypeKind>],
) -> Option<Rc<TypeKind>> {
    actual.filter(|kind| expected.contains(kind))
}

/// Creates the union of two characteristics; if right-hand-side is `None`, it
/// returns the left-hand-side
fn characteristics_union(
    lhs: Rc<Characteristics>,
    rhs: &Option<Rc<Characteristics>>,
) -> Rc<Characteristics> {
    if let Some(rhs) = rhs {
        Characteristics::union(lhs, rhs.clone())
    } else {
        lhs
    }
}

fn augment_callable_type_kind(
    type_kind: &TypeKind,
    characteristic: Rc<Characteristics>,
) -> Option<Rc<TypeKind>> {
    match type_kind {
        TypeKind::Operation(args, return_type, rhs) => {
            Some(TypeKind::operation_with_characteristics(
                args.clone(),
                return_type.clone(),
                characteristics_union(characteristic, rhs),
            ))
        }
        TypeKind::Function(args, return_type, rhs) => {
            Some(TypeKind::function_with_characteristics(
                args.clone(),
                return_type.clone(),
                characteristics_union(characteristic, rhs),
            ))
        }
        _ => None,
    }
}

#[allow(dead_code)]
pub(crate) fn type_from_expression(
    expr: &Expression,
    symbol_table: &HashMap<QualifiedName, Rc<TypeKind>>,
) -> Option<Rc<TypeKind>> {
    match expr {
        Expression::UnitValue => Some(TypeKind::unit()),
        Expression::Missing => todo!(),
        Expression::Identifier(name, None) => symbol_table.get(name).cloned(),
        Expression::IntLiteral(_) => Some(TypeKind::int()),
        Expression::BigIntLiteral(_) => Some(TypeKind::big_int()),
        Expression::DoubleLiteral(_) => Some(TypeKind::double()),
        Expression::BoolLiteral(_) => Some(TypeKind::bool()),
        Expression::StringLiteral(_) => Some(TypeKind::string()),
        Expression::PauliLiteral(_) => Some(TypeKind::pauli()),
        Expression::ResultLiteral(_) => Some(TypeKind::result()),
        Expression::ArrayLiteral(items) => {
            let first = items.first()?;

            // all items must be of the same type
            if items.iter().skip(1).any(|expr| expr != first) {
                return None;
            }

            Some(TypeKind::array(type_from_expression(first, symbol_table)?))
        }
        Expression::SizedArrayLiteral(item, size) => {
            with_type_check(type_from_expression(size, symbol_table), &TypeKind::int())?;

            Some(TypeKind::array(type_from_expression(item, symbol_table)?))
        }
        Expression::TupleLiteral(items) => {
            let mut types = vec![];

            for item in items {
                types.push(TypeTupleItem::simple(type_from_expression(
                    item,
                    symbol_table,
                )?));
            }

            Some(TypeKind::tuple(types))
        }
        Expression::NewLiteral(kind, expr) => {
            with_type_check(type_from_expression(expr, symbol_table), &TypeKind::int())?;

            Some(TypeKind::array(kind.clone()))
        }

        Expression::PosPrefix(expr)
        | Expression::NegPrefix(expr)
        | Expression::BitwiseNot(expr) => with_type_check_in(
            type_from_expression(expr, symbol_table),
            &[TypeKind::int(), TypeKind::big_int()],
        ),
        Expression::LogicalNot(expr) => {
            with_type_check(type_from_expression(expr, symbol_table), &TypeKind::bool())
        }
        Expression::Adjoint(expr) => augment_callable_type_kind(
            type_from_expression(expr, symbol_table)?.as_ref(),
            Characteristics::adj(),
        ),
        Expression::Controlled(expr) => augment_callable_type_kind(
            type_from_expression(expr, symbol_table)?.as_ref(),
            Characteristics::ctl(),
        ),

        Expression::Range(_lhs, _rhs) => todo!(),
        Expression::LogicalOr(lhs, rhs) | Expression::LogicalAnd(lhs, rhs) => {
            let lhs = type_from_expression(lhs, symbol_table);
            let rhs = type_from_expression(rhs, symbol_table);
            if lhs == rhs {
                with_type_check(lhs, &TypeKind::bool())
            } else {
                None
            }
        }
        Expression::Equality(lhs, rhs) | Expression::Inequality(lhs, rhs) => {
            // TODO type check
            let lhs = type_from_expression(lhs, symbol_table);
            let rhs = type_from_expression(rhs, symbol_table);
            if lhs == rhs {
                lhs
            } else {
                None
            }
        }
        Expression::BitwiseOr(lhs, rhs)
        | Expression::BitwiseXor(lhs, rhs)
        | Expression::BitwiseAnd(lhs, rhs)
        | Expression::Modulus(lhs, rhs) => {
            let lhs = type_from_expression(lhs, symbol_table);
            let rhs = type_from_expression(rhs, symbol_table);
            if lhs == rhs {
                with_type_check_in(lhs, &[TypeKind::int(), TypeKind::big_int()])
            } else {
                None
            }
        }
        Expression::LessThan(lhs, rhs)
        | Expression::LessThanOrEqual(lhs, rhs)
        | Expression::GreaterThan(lhs, rhs)
        | Expression::GreaterThanOrEqual(lhs, rhs)
        | Expression::Subtraction(lhs, rhs)
        | Expression::Multiplication(lhs, rhs)
        | Expression::Division(lhs, rhs)
        | Expression::Exponentiation(lhs, rhs) => {
            let lhs = type_from_expression(lhs, symbol_table);
            let rhs = type_from_expression(rhs, symbol_table);
            if lhs == rhs {
                with_type_check_in(
                    lhs,
                    &[TypeKind::int(), TypeKind::big_int(), TypeKind::double()],
                )
            } else {
                None
            }
        }
        Expression::RightShift(lhs, rhs) | Expression::LeftShift(lhs, rhs) => {
            with_type_check(type_from_expression(rhs, symbol_table), &TypeKind::int())?;
            with_type_check_in(
                type_from_expression(lhs, symbol_table),
                &[TypeKind::int(), TypeKind::big_int()],
            )
        }
        Expression::Addition(lhs, rhs) => {
            let lhs = type_from_expression(lhs, symbol_table)?;
            let rhs = type_from_expression(rhs, symbol_table)?;

            match (lhs.as_ref(), rhs.as_ref()) {
                (TypeKind::Int, TypeKind::Int)
                | (TypeKind::BigInt, TypeKind::BigInt)
                | (TypeKind::Double, TypeKind::Double) => Some(lhs),
                (TypeKind::Array(i1), TypeKind::Array(i2)) if i1 == i2 => Some(lhs),
                _ => None,
            }
        }

        _ => todo!(),
    }
}
