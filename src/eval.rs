use crate::parser;
use crate::parser::ParseError;
use std::error::Error;
use std::fmt::Debug;
use std::str::FromStr;
use thiserror::Error;

/// Parses an expression from a string and evaluates it.
pub fn eval<T: Eval>(expression: &str) -> Result<T, EvalError<T>> {
    parser::parse::<T>(expression)?
        .eval()
        .map_err(|err| EvalError::EvalError(err))
}

/// An error returned by the [eval()] function.
#[derive(Error, Debug, PartialEq)]
pub enum EvalError<T: Eval> {
    /// The expression could not be parsed correctly.
    #[error("could not parse expression: {0}")]
    ParseError(#[from] ParseError<T>),
    /// Something went wrong trying to evaluate a correctly parsed expression. This is usually a
    /// mathematical error (such as division by zero) or a custom defined error specific to the type
    /// that used for evaluating the expression.
    #[error("could not evaluate expression: {0}")]
    EvalError(<T as Eval>::ErrEval),
}

/// Eval is a trait that allows a type to be used to evaluate an expression.
///
/// This type should be parsable from a string that matches the following regex: `[A-Za-z0-9_.,]+`.
/// Note that `-` is not included in here. Negative numbers can be created using the unary minus
/// operator.
///
/// It is not recommended to panic in any these methods, but instead return an error when an
/// operation was unsuccessful. This way, the error can be properly handled and clearly shown.
pub trait Eval: EvalParse + Clone {
    /// The error type to return if an operation cannot be done successfully when evaluating an
    /// expression.
    type ErrEval: Error + PartialEq;

    /// An operation of the form: `a + b`.
    fn add(self, other: Self) -> Result<Self, Self::ErrEval>;
    /// An operation of the form: `a - b`.
    fn sub(self, other: Self) -> Result<Self, Self::ErrEval>;
    /// An operation of the form: `a * b`.
    fn mul(self, other: Self) -> Result<Self, Self::ErrEval>;
    /// An operation of the form: `a / b`.
    fn div(self, other: Self) -> Result<Self, Self::ErrEval>;
    /// An operation of the form: `a % b`.
    fn rem(self, other: Self) -> Result<Self, Self::ErrEval>;
    /// An operation of the form: `a ^ b`.
    fn exp(self, other: Self) -> Result<Self, Self::ErrEval>;

    /// An operation of the form: `+a`.
    fn plus(self) -> Result<Self, Self::ErrEval>;
    /// An operation of the form: `-a`.
    fn minus(self) -> Result<Self, Self::ErrEval>;
    /// An operation of the form: `!a`.
    fn not(self) -> Result<Self, Self::ErrEval>;
}

/// A trait to allow a literal to be parsed, guaranteeing that the error implements [Error] +
/// [PartialEq].
pub trait EvalParse: Sized {
    /// The error type to return if a literal could not be parsed as the type.
    type ErrParse: Error + PartialEq;

    /// Parse returns a value of type `Self` parsed from a literal. This method exists rather than
    /// using the [FromStr] trait because [FromStr::Err] does not implement error.
    fn parse(literal: &str) -> Result<Self, Self::ErrParse>;
}

impl<T: FromStr + Sized> EvalParse for T
where
    <T as FromStr>::Err: Error + PartialEq,
{
    type ErrParse = <T as FromStr>::Err;

    fn parse(literal: &str) -> Result<T, Self::ErrParse> {
        literal.parse()
    }
}

/// Macro to facilitate implementing the [Eval] trait for every single integer type.
macro_rules! eval_int {
    ($i:ty) => {
        impl Eval for $i {
            type ErrEval = IntEvalError;

            fn add(self, other: Self) -> Result<Self, Self::ErrEval> {
                self.checked_add(other).ok_or(IntEvalError::Overflow)
            }

            fn sub(self, other: Self) -> Result<Self, Self::ErrEval> {
                self.checked_sub(other).ok_or(IntEvalError::Overflow)
            }

            fn mul(self, other: Self) -> Result<Self, Self::ErrEval> {
                self.checked_mul(other).ok_or(IntEvalError::Overflow)
            }

            fn div(self, other: Self) -> Result<Self, Self::ErrEval> {
                self.checked_div(other).ok_or(IntEvalError::DivideByZero)
            }

            fn rem(self, other: Self) -> Result<Self, Self::ErrEval> {
                self.checked_rem(other).ok_or(IntEvalError::DivideByZero)
            }

            fn exp(self, other: Self) -> Result<Self, Self::ErrEval> {
                self.checked_pow(other.try_into().map_err(|_| IntEvalError::Overflow)?)
                    .ok_or(IntEvalError::Overflow)
            }

            fn plus(self) -> Result<Self, Self::ErrEval> {
                Ok(self)
            }

            fn minus(self) -> Result<Self, Self::ErrEval> {
                self.checked_neg().ok_or(IntEvalError::Overflow)
            }

            fn not(self) -> Result<Self, Self::ErrEval> {
                Ok(!self)
            }
        }
    };
}

/// An error that can occur when evaluating an expression with integers.
#[derive(Error, Debug, PartialEq)]
pub enum IntEvalError {
    /// When the divisor is equals to zero, a DivideByZero error is returned.
    #[error("division by zero")]
    DivideByZero,
    /// When the number becomes too large or too small, an Overflow error is returned.
    #[error("operation resulted in overflow")]
    Overflow,
}

eval_int!(u8);
eval_int!(u16);
eval_int!(u32);
eval_int!(u64);
eval_int!(u128);
eval_int!(usize);

eval_int!(i8);
eval_int!(i16);
eval_int!(i32);
eval_int!(i64);
eval_int!(i128);
eval_int!(isize);

/// Macro to facilitate implementing the [Eval] trait for every single float type.
macro_rules! eval_float {
    ($i:ty) => {
        impl Eval for $i {
            type ErrEval = FloatEvalError;

            fn add(self, other: Self) -> Result<Self, Self::ErrEval> {
                Ok(self + other)
            }

            fn sub(self, other: Self) -> Result<Self, Self::ErrEval> {
                Ok(self - other)
            }

            fn mul(self, other: Self) -> Result<Self, Self::ErrEval> {
                Ok(self * other)
            }

            fn div(self, other: Self) -> Result<Self, Self::ErrEval> {
                Ok(self / other)
            }

            fn rem(self, other: Self) -> Result<Self, Self::ErrEval> {
                Ok(self % other)
            }

            fn exp(self, other: Self) -> Result<Self, Self::ErrEval> {
                Ok(self.powf(other))
            }

            fn plus(self) -> Result<Self, Self::ErrEval> {
                Ok(self)
            }

            fn minus(self) -> Result<Self, Self::ErrEval> {
                Ok(-self)
            }

            fn not(self) -> Result<Self, Self::ErrEval> {
                Err(FloatEvalError::UnsupportedOperation("!"))
            }
        }
    };
}

/// An error that can occur when evaluating an expression with floating point numbers.
#[derive(Error, Debug, PartialEq)]
pub enum FloatEvalError {
    /// Not all operations are supported on floating pointer numbers (notably bitwise operations).
    #[error("use of unsupported operator: {0}")]
    UnsupportedOperation(&'static str),
}

eval_float!(f32);
eval_float!(f64);
