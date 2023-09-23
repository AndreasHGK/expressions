use std::fmt::{Display, Formatter};
use std::io::{stdin, stdout, Write};
use std::num::ParseFloatError;
use std::str::FromStr;

use thiserror::Error;

use expressions::eval;
use expressions::eval::Eval;

/// A complex number that uses an f64 for its real and imaginary part.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Complex {
    pub real: f64,
    pub imag: f64,
}

impl Complex {
    /// Helper function to create complex numbers.
    pub fn new(real: f64, imag: f64) -> Self {
        Self { real, imag }
    }
}

impl Display for Complex {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match (self.real != 0., self.imag != 0.) {
            (true, true) => f.write_str(&format!(
                "{} {} {}i",
                self.real,
                if self.imag.is_sign_positive() {
                    '+'
                } else {
                    '-'
                },
                self.imag.abs()
            )),
            (true, false) => f.write_str(&format!("{}", self.real)),
            (false, true) => f.write_str(&format!("{}i", self.imag)),
            (false, false) => f.write_str("0"),
        }
    }
}

/// An error that occurs when a successfully parsed expression can not be evaluated successfully.
#[derive(Error, Debug, PartialEq)]
pub enum ComplexEvalErr {
    /// An operator that is not supported on complex numbers was used.
    #[error("use of unsupported operator")]
    OperatorNotSupported,
}

/// Implementing eval allows the type to be used as type to evaluate expressions with. To implement
/// eval, [Clone] and [eval::EvalParse] should also be implemented on the type.
impl Eval for Complex {
    type ErrEval = ComplexEvalErr;

    fn eq(self, other: Self) -> Result<Self, Self::ErrEval> {
        match self == other {
            true => Ok(Self::new(1., 0.)),
            false => Ok(Self::new(0., 0.)),
        }
    }

    fn neq(self, other: Self) -> Result<Self, Self::ErrEval> {
        match self != other {
            true => Ok(Self::new(1., 0.)),
            false => Ok(Self::new(0., 0.)),
        }
    }

    fn gte(self, _other: Self) -> Result<Self, Self::ErrEval> {
        Err(ComplexEvalErr::OperatorNotSupported)
    }

    fn gt(self, _other: Self) -> Result<Self, Self::ErrEval> {
        Err(ComplexEvalErr::OperatorNotSupported)
    }

    fn lte(self, _other: Self) -> Result<Self, Self::ErrEval> {
        Err(ComplexEvalErr::OperatorNotSupported)
    }

    fn lt(self, _other: Self) -> Result<Self, Self::ErrEval> {
        Err(ComplexEvalErr::OperatorNotSupported)
    }

    fn and(self, _other: Self) -> Result<Self, Self::ErrEval> {
        Err(ComplexEvalErr::OperatorNotSupported)
    }

    fn or(self, _other: Self) -> Result<Self, Self::ErrEval> {
        Err(ComplexEvalErr::OperatorNotSupported)
    }

    fn bit_and(self, _other: Self) -> Result<Self, Self::ErrEval> {
        Err(ComplexEvalErr::OperatorNotSupported)
    }

    fn bit_or(self, _other: Self) -> Result<Self, Self::ErrEval> {
        Err(ComplexEvalErr::OperatorNotSupported)
    }

    fn add(self, other: Self) -> Result<Self, Self::ErrEval> {
        Ok(Self {
            real: self.real + other.real,
            imag: self.imag + other.imag,
        })
    }

    fn sub(self, other: Self) -> Result<Self, Self::ErrEval> {
        Ok(Self {
            real: self.real - other.real,
            imag: self.imag - other.imag,
        })
    }

    fn mul(self, other: Self) -> Result<Self, Self::ErrEval> {
        Ok(Self {
            real: self.real * other.real - self.imag * other.imag,
            imag: self.real * other.imag * 2.,
        })
    }

    fn div(self, other: Self) -> Result<Self, Self::ErrEval> {
        let divisor = other.real * other.real + other.imag * other.imag;
        Ok(Self {
            real: (self.real * other.real + self.imag * other.imag) / divisor,
            imag: (self.imag * other.real - self.real * other.imag) / divisor,
        })
    }

    fn rem(self, _: Self) -> Result<Self, Self::ErrEval> {
        Err(ComplexEvalErr::OperatorNotSupported)
    }

    fn exp(self, _: Self) -> Result<Self, Self::ErrEval> {
        Err(ComplexEvalErr::OperatorNotSupported)
    }

    fn plus(self) -> Result<Self, Self::ErrEval> {
        Ok(self)
    }

    fn minus(self) -> Result<Self, Self::ErrEval> {
        Ok(Self {
            real: -self.real,
            imag: -self.imag,
        })
    }

    fn not(self) -> Result<Self, Self::ErrEval> {
        Err(ComplexEvalErr::OperatorNotSupported)
    }

    fn bit_not(self) -> Result<Self, Self::ErrEval> {
        Err(ComplexEvalErr::OperatorNotSupported)
    }
}

/// An error that occurs when a complex number literal could not be parsed.
#[derive(Error, Debug, PartialEq)]
pub enum ComplexParseErr {
    /// The float literal could not be parsed regardless of whether or not 'i' was present.
    #[error("{0}")]
    ParseFloatErr(#[from] ParseFloatError),
}

/// [eval::EvalParse] is automatically implemented for any type that implements [FromStr] where the
/// returned error implements [std::error::Error].
impl FromStr for Complex {
    type Err = ComplexParseErr;

    /// Parses the complex number from a string. Formats such as `a + bi` need not be supported:
    /// these values can be achieved with an binary addition of two literals `a` and `bi`.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Remove the 'i' from the string if it is an imaginary number.
        let (substr, is_imag) = if s.chars().last().map_or(false, |v| v == 'i') {
            (&s[0..(s.len() - 1)], true)
        } else {
            (s, false)
        };

        let magnitude = substr.parse()?;
        Ok(Self {
            real: if !is_imag { magnitude } else { 0. },
            imag: if is_imag { magnitude } else { 0. },
        })
    }
}

#[test]
fn test_complex() {
    assert_eq!(
        eval::<Complex>("-1i + 23 - 394"),
        Ok(Complex::new(-371., -1.))
    );
    assert_eq!(
        eval::<Complex>("-1i + 23 + (2i - --1i)"),
        Ok(Complex::new(23., 0.))
    );
    assert_eq!(
        eval::<Complex>("(-1i + 23 + 2i - --1i) * (1 - 3i) * (5i)"),
        Ok(Complex::new(690., 230.))
    );
    assert_eq!(eval::<Complex>("10 * 10"), Ok(Complex::new(100., 0.)),);
    assert_eq!(eval::<Complex>("10i * 10i"), Ok(Complex::new(-100., 0.)),);
    assert_eq!(
        eval::<Complex>("-1i + 23 - 390 / 39"),
        Ok(Complex::new(13., -1.))
    );
    assert_eq!(eval::<Complex>("-1i / 1i"), Ok(Complex::new(-1., 0.)));

    assert!(eval::<Complex>("-1ii").is_err());
    assert!(eval::<Complex>("i").is_err());
    assert!(eval::<Complex>("").is_err());
}

fn main() {
    let mut buf = String::new();
    loop {
        print!("Input: ");
        stdout().flush().expect("could not flush stdout");
        stdin()
            .read_line(&mut buf)
            .expect("could not read from stdin: ");

        match eval::<Complex>(&buf) {
            Ok(val) => println!("Result: {}", val),
            Err(err) => eprintln!("Error: {}", err),
        };
        buf.clear();
    }
}
