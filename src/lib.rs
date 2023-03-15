#![doc = include_str!("../README.md")]

pub mod ast;
pub mod eval;
pub mod parser;

pub use eval::eval;

#[cfg(test)]
mod tests {
    use crate::ast::{BinaryOp, BinaryOpType, Expression, Span, Spanned};
    use crate::eval::{eval, Eval, EvalError, IntEvalError};
    use crate::parser::parse;

    #[test]
    fn test_eval() {
        /// Writes an `assert_eq` for an expressions.
        /// This macro writes the following line for every expression:
        /// ```ignore
        /// assert_eq!(parse(stringify!($e)).unwrap().eval(), $e);
        /// ```
        /// This just exists as to not accidentally mistype an expression.
        macro_rules! try_eval (
            ($i:ty, $e:expr $(, $extra:expr)* $(,)?) => {
                assert_eq!(eval::<$i>(stringify!($e)).unwrap(), $e);
            };
        );

        try_eval!(f32, 100. / 2. + 29. % 2. % 2.);
        try_eval!(f32, 12.394 / 1.23 * 0234.0);

        try_eval!(i32, 1 + 1);
        try_eval!(i32, 1 + 3 + 10 * 118 / 12 + 4 % 2);
        try_eval!(i32, 2393478 - 380 + 234 / 736);
        try_eval!(i32, 100);
        try_eval!(i32, 1 - -1 + (-----1 * (4 * -3 % -3)) * 10);
        try_eval!(i32, 1 - -1 + ((-----1) * (4 * -3 % -3)) * 10);
        try_eval!(i32, 1 - -1 + (-----1 * (4 * -3 % -3)) + 10);
        try_eval!(i32, 2 * 2 * 2 * 2 * 2 * 2 * 2);
        try_eval!(i32, 2 * 2 * 2 - (2 * 2 * 2) * 2);
        try_eval!(i32, 100 / 2 + 29 % 2 % 2);
        assert_eq!(
            eval::<i32>("2 ^ 3 ^ 1 + 4").unwrap(),
            2i32.exp(3.exp(1).unwrap()).unwrap() + 4,
        );
        assert_eq!(
            eval::<i32>("2 ^ 3 ^ (1 + 1)").unwrap(),
            2i32.exp(3.exp(1 + 1).unwrap()).unwrap(),
        );
    }

    #[test]
    fn test_eval_err() {
        assert_eq!(
            eval::<i32>(&format!("{} + 1", i32::MAX)),
            Err(EvalError::EvalError(IntEvalError::Overflow))
        );
        assert_eq!(
            eval::<i32>("1 + 1 / (1 - 1)"),
            Err(EvalError::EvalError(IntEvalError::DivideByZero))
        );
    }

    #[test]
    fn test_parse() {
        assert_eq!(
            parse::<f32>("78").unwrap(),
            Expression::Literal(Spanned::new(78., Span::new(0, 2).unwrap())),
        );
        assert_eq!(
            parse::<i32>("(1)").unwrap(),
            Expression::Literal(Spanned::new(1i32, Span::new(1, 2).unwrap())),
        );
        assert_eq!(
            parse::<i32>("((((((((((((((((1))))))))))))))))").unwrap(),
            Expression::Literal(Spanned::new(1i32, Span::new(16, 17).unwrap())),
        );
        assert_eq!(
            parse::<i32>("(2 ^ 3) ^ (11 + 1)").unwrap(),
            Expression::BinaryOp(Box::new(BinaryOp {
                operand1: Expression::BinaryOp(Box::new(BinaryOp {
                    operand1: Expression::Literal(Spanned::new(2, Span::new(1, 2).unwrap())),
                    operand2: Expression::Literal(Spanned::new(3, Span::new(5, 6).unwrap())),
                    operator: Spanned::new(BinaryOpType::Exp, Span::new(3, 4).unwrap()),
                })),
                operand2: Expression::BinaryOp(Box::new(BinaryOp {
                    operand1: Expression::Literal(Spanned::new(11, Span::new(11, 13).unwrap())),
                    operand2: Expression::Literal(Spanned::new(1, Span::new(16, 17).unwrap())),
                    operator: Spanned::new(BinaryOpType::Add, Span::new(14, 15).unwrap()),
                })),
                operator: Spanned::new(BinaryOpType::Exp, Span::new(8, 9).unwrap()),
            })),
        );
        assert_eq!(
            parse::<i32>("2 ^ 3 ^ (11 + 1)").unwrap(),
            Expression::BinaryOp(Box::new(BinaryOp {
                operand1: Expression::Literal(Spanned::new(2, Span::new(0, 1).unwrap())),
                operand2: Expression::BinaryOp(Box::new(BinaryOp {
                    operand1: Expression::Literal(Spanned::new(3, Span::new(4, 5).unwrap())),
                    operand2: Expression::BinaryOp(Box::new(BinaryOp {
                        operand1: Expression::Literal(Spanned::new(11, Span::new(9, 11).unwrap())),
                        operand2: Expression::Literal(Spanned::new(1, Span::new(14, 15).unwrap())),
                        operator: Spanned::new(BinaryOpType::Add, Span::new(12, 13).unwrap()),
                    })),
                    operator: Spanned::new(BinaryOpType::Exp, Span::new(6, 7).unwrap()),
                })),
                operator: Spanned::new(BinaryOpType::Exp, Span::new(2, 3).unwrap()),
            })),
        );
    }

    #[test]
    fn test_parse_err() {
        assert!(parse::<i32>("1 + 1 +").is_err());
        assert!(parse::<i32>("").is_err());
        assert!(parse::<i32>("(1").is_err());
        assert!(parse::<i32>("1 + 2 + * 2").is_err());
        assert!(parse::<i32>("* 1").is_err());
        assert!(parse::<i32>("1 ^ 1 ^ 1 )(").is_err());
        assert!(parse::<i32>(&format!("{}1", i32::MAX)).is_err());
        assert!(parse::<i32>("1 + abc").is_err());
    }
}
