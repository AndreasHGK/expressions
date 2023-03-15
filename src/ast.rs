use crate::eval::Eval;
use pest::error::InputLocation;
use std::fmt::{Debug, Formatter};
use std::ops::{Deref, DerefMut};

/// A tree representation of a mathematical expression.
#[derive(Debug, Clone)]
pub enum Expression<T: Eval> {
    /// Literals are constant values that have not had an operation applied to them.
    ///
    /// For integers this is simply `1`, `2`, ...
    Literal(Spanned<T>),
    /// See [BinaryOp].
    BinaryOp(Box<BinaryOp<T>>),
    /// See [UnaryOp].
    UnaryOp(Box<UnaryOp<T>>),
}

impl<T: Eval> Expression<T> {
    /// Evaluates the value of the expression.
    pub fn eval(&self) -> Result<T, T::ErrEval> {
        match self {
            Expression::Literal(v) => Ok(v.deref().clone()),
            Expression::BinaryOp(v) => v.eval(),
            Expression::UnaryOp(v) => v.eval(),
        }
    }

    /// Indicates where the expression is in the source string it was parsed from.
    pub fn span(&self) -> Span {
        match self {
            Expression::Literal(v) => v.span(),
            Expression::BinaryOp(v) => v.span(),
            Expression::UnaryOp(v) => v.span(),
        }
    }
}

impl<T: Eval + PartialEq> PartialEq for Expression<T> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Expression::Literal(v1) => {
                if let Expression::Literal(v2) = other {
                    v1 == v2
                } else {
                    false
                }
            }
            Expression::BinaryOp(v1) => {
                if let Expression::BinaryOp(v2) = other {
                    v1 == v2
                } else {
                    false
                }
            }
            Expression::UnaryOp(v1) => {
                if let Expression::UnaryOp(v2) = other {
                    v1 == v2
                } else {
                    false
                }
            }
        }
    }
}

/// A binary operation is an operation between the resulting value of two expressions.
///
/// Addition `a + b` is an example of a binary operation, since it requires two values.
#[derive(Debug, Clone)]
pub struct BinaryOp<T: Eval> {
    /// The first operand of the expression. This is the operand on the left hand side.
    pub operand1: Expression<T>,
    /// The second operand of the expression. This is the operand on the right hand side.
    pub operand2: Expression<T>,
    /// See [BinaryOpType]
    pub operator: Spanned<BinaryOpType>,
}

impl<T: Eval> BinaryOp<T> {
    /// Evaluates the value of the binary operation.
    pub fn eval(&self) -> Result<T, T::ErrEval> {
        let val1 = self.operand1.eval()?;
        let val2 = self.operand2.eval()?;

        match self.operator.deref() {
            BinaryOpType::Add => val1.add(val2),
            BinaryOpType::Sub => val1.sub(val2),
            BinaryOpType::Mul => val1.mul(val2),
            BinaryOpType::Div => val1.div(val2),
            BinaryOpType::Mod => val1.rem(val2),
            BinaryOpType::Exp => val1.exp(val2),
        }
    }

    /// Indicates where the expression is in the source string it was parsed from.
    pub fn span(&self) -> Span {
        Span::combine(
            [
                self.operand1.span(),
                self.operand2.span(),
                self.operator.span(),
            ]
            .as_ref(),
        )
        .unwrap()
    }
}

impl<T: Eval + PartialEq> PartialEq for BinaryOp<T> {
    fn eq(&self, other: &Self) -> bool {
        self.operator == other.operator
            && self.operand1 == other.operand1
            && self.operand2 == other.operand2
    }
}

/// The kind of binary operation. Examples of such types are addition or subtraction.
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOpType {
    /// `a + b`
    Add,
    /// `a - b`
    Sub,
    /// `a * b`
    Mul,
    /// `a / b`
    Div,
    /// `a % b`
    Mod,
    /// `a ^ b`
    Exp,
}

/// A unary operation is an operation that applies on a single expression.
///
/// An example of such an operation is the negate operation: `-a`.
#[derive(Debug, Clone)]
pub struct UnaryOp<T: Eval> {
    /// The expression on which the operation applies.
    pub operand: Expression<T>,
    /// See [UnaryOpType]
    pub operator: Spanned<UnaryOpType>,
}

impl<T: Eval> UnaryOp<T> {
    /// Evaluates the value of the unary operation.
    pub fn eval(&self) -> Result<T, T::ErrEval> {
        let val = self.operand.eval()?;

        match self.operator.deref() {
            UnaryOpType::Plus => val.plus(),
            UnaryOpType::Minus => val.minus(),
            UnaryOpType::Not => val.not(),
        }
    }

    /// Indicates where the expression is in the source string it was parsed from.
    pub fn span(&self) -> Span {
        Span::combine([self.operand.span(), self.operator.span()].as_ref()).unwrap()
    }
}

impl<T: Eval + PartialEq> PartialEq for UnaryOp<T> {
    fn eq(&self, other: &Self) -> bool {
        self.operator == other.operator && self.operand == other.operand
    }
}

/// The kind of unary operation.
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOpType {
    /// `+a`
    Plus,
    /// `-a`
    Minus,
    /// `!a`
    Not,
}

/// A span denotes an character interval in the source expression string.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    /// Safely create a new span. Will return none if `end <= start`.
    pub fn new(start: usize, end: usize) -> Option<Span> {
        if end <= start {
            return None;
        }
        Some(Self { start, end })
    }

    /// Combines multiple spans into one span that spans the union of all spans in the provided
    /// vector, as well as any gaps in between those spans.
    ///
    /// Returns none if no spans are provided.
    pub fn combine<'a>(spans: impl Into<&'a [Span]>) -> Option<Span> {
        let spans = spans.into();
        if spans.is_empty() {
            return None;
        }

        let mut start = spans[0].start;
        let mut end = spans[0].end;
        for span in spans {
            if span.start < start {
                start = span.start
            }
            if span.end > end {
                end = span.end
            }
        }
        Some(Span { start, end })
    }

    /// The first character contained in the span.
    pub fn start(&self) -> usize {
        self.start
    }

    /// The end position of the span. This is the first character that is no longer included in the
    /// span.
    pub fn end(&self) -> usize {
        self.end
    }

    /// Returns the amount of characters in the span. A valid span is always at least one character
    /// in length.
    pub fn len(&self) -> usize {
        self.end - self.start
    }
}

impl From<InputLocation> for Span {
    fn from(value: InputLocation) -> Self {
        match value {
            InputLocation::Pos(v) => Span::new(v, v + 1).unwrap(),
            InputLocation::Span((v1, v2)) => Span::new(v1, v2 + 1).unwrap(),
        }
    }
}

/// Wrapper that allows an arbitrary type to have a span.
pub struct Spanned<T: Sized> {
    span: Span,
    inner: T,
}

impl<T: Sized> Spanned<T> {
    /// Creates a new wrapper with the provided value and span.
    pub fn new(inner: T, span: Span) -> Self {
        Self { span, inner }
    }

    /// Unwraps the span, returning the inner value.
    pub fn into_inner(self) -> T {
        self.inner
    }

    /// Returns the span associated with the inner value.
    pub fn span(&self) -> Span {
        self.span
    }
}

impl<T: Sized + Clone> Clone for Spanned<T> {
    fn clone(&self) -> Self {
        Self {
            span: self.span.clone(),
            inner: self.inner.clone(),
        }
    }
}

impl<T: Sized + PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.span == other.span && self.inner == other.inner
    }
}

impl<T: Sized + Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(format!("Spanned({:?}, {:?})", self.span, self.inner).as_str())
    }
}

impl<T: Sized> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T: Sized> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl From<pest::Span<'_>> for Span {
    fn from(value: pest::Span<'_>) -> Self {
        Span::new(value.start(), value.end()).expect("could not convert span")
    }
}
