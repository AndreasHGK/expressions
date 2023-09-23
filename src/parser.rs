use crate::ast::{BinaryOp, BinaryOpType, Expression, Span, Spanned, UnaryOp, UnaryOpType};
use crate::eval::Eval;
use lazy_static::lazy_static;
use pest::error::{Error, ErrorVariant};
use pest::iterators::Pairs;
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest::Parser as _;
use std::fmt::{Debug, Display, Formatter};
use thiserror::Error;

/// Parses a string into an [Expression] tree.
pub fn parse<T: Eval>(input: &str) -> Result<Expression<T>, ParseError<T>> {
    Ok(parse_expression(
        Parser::parse(Rule::root, input)?
            .peek()
            .unwrap() // The `.unwrap()` is safe: root will always contain an expression.
            .into_inner(),
    )?)
}

/// An error that occurs while parsing an expression from a string.
#[derive(Error, Debug, PartialEq)]
pub enum ParseError<T: Eval> {
    /// An error that occurred while calling the literal's own parse function.
    #[error("could not parse literal: {0}")]
    LiteralParseError(T::ErrParse),
    /// See [SyntaxError].
    #[error("syntax error: {0}")]
    SyntaxError(#[from] SyntaxError),
}

/// When there is a problem with the syntax while parsing the expression, a SyntaxError is returned.
///
/// It contains info about where the error took place and what could be done to fix it.
#[derive(Debug, PartialEq)]
pub struct SyntaxError {
    /// The location of the erroneous character(s) in the input string.
    pub location: Span,
    /// The set of expected tokens or rules.
    pub expected: Vec<&'static str>,
}

impl Display for SyntaxError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!(
            "syntax error from position {} to {}: expected {}",
            self.location.start(),
            self.location.end(),
            self.expected.join(", "),
        ))
    }
}

impl From<Error<Rule>> for SyntaxError {
    fn from(err: Error<Rule>) -> Self {
        let mut expected = Vec::new();
        if let ErrorVariant::ParsingError { positives, .. } = err.variant {
            for positive in positives {
                // Explicitly dont use the default `_ => None` here to not forget any rules.
                let val = match positive {
                    Rule::LITERAL => Some("literal"),
                    Rule::expression => Some("expression"),

                    Rule::EQ => Some("'=='"),
                    Rule::NEQ => Some("'!='"),
                    Rule::GTE => Some("'>='"),
                    Rule::GT => Some("'>'"),
                    Rule::LTE => Some("'<='"),
                    Rule::LT => Some("'<'"),
                    Rule::AND => Some("'&&'"),
                    Rule::OR => Some("'||'"),
                    Rule::BIT_AND => Some("'&'"),
                    Rule::BIT_OR => Some("'|'"),
                    Rule::ADD => Some("'+'"),
                    Rule::SUB => Some("'-'"),
                    Rule::MUL => Some("'*'"),
                    Rule::DIV => Some("'/'"),
                    Rule::MOD => Some("'%'"),
                    Rule::EXP => Some("'^'"),

                    Rule::PLUS => Some("'+'"),
                    Rule::MINUS => Some("'-'"),
                    Rule::NOT => Some("'!'"),
                    Rule::BIT_NOT => Some("'~'"),

                    Rule::DELIM_L => Some("'('"),
                    Rule::DELIM_R => Some("')'"),

                    Rule::EOI => None,
                    Rule::WHITESPACE => None,
                    Rule::root => None,
                    Rule::group => None,
                    Rule::simple => None,
                    Rule::binary_op => None,
                    Rule::binary_operator => None,
                    Rule::unary_op => None,
                    Rule::unary_operator => None,
                };
                if let Some(v) = val {
                    expected.push(v);
                }
            }
        }
        SyntaxError {
            location: err.location.into(),
            expected,
        }
    }
}

impl<T: Eval> From<Error<Rule>> for ParseError<T> {
    fn from(value: Error<Rule>) -> Self {
        SyntaxError::from(value).into()
    }
}

impl std::error::Error for SyntaxError {}

/// Implementation for the expression grammar parser.
#[derive(pest_derive::Parser)]
#[grammar = "./src/expression.pest"]
struct Parser;

lazy_static! {
    /// A pratt parser with the correct order of operations configured to avoid having to create it
    /// every time it is needed.
    static ref PARSER: PrattParser<Rule> = {
        PrattParser::new()
            .op(
                Op::infix(Rule::EQ, Assoc::Left)
                | Op::infix(Rule::NEQ, Assoc::Left)
                | Op::infix(Rule::GTE, Assoc::Left)
                | Op::infix(Rule::GT, Assoc::Left)
                | Op::infix(Rule::LTE, Assoc::Left)
                | Op::infix(Rule::LT, Assoc::Left)
            )
            .op(Op::infix(Rule::AND, Assoc::Left) | Op::infix(Rule::OR, Assoc::Left))
            .op(Op::infix(Rule::BIT_AND, Assoc::Left) | Op::infix(Rule::BIT_OR, Assoc::Left))
            .op(Op::infix(Rule::ADD, Assoc::Left) | Op::infix(Rule::SUB, Assoc::Left))
            .op(
                Op::infix(Rule::MUL, Assoc::Left)
                | Op::infix(Rule::DIV, Assoc::Left)
                | Op::infix(Rule::MOD, Assoc::Left)
            )
            .op(Op::infix(Rule::EXP, Assoc::Right))
            .op(Op::prefix(Rule::PLUS) | Op::prefix(Rule::MINUS) | Op::prefix(Rule::NOT) | Op::prefix(Rule::BIT_NOT))
    };
}

/// An internal function to convert the parser's CST into an [Expression].
fn parse_expression<T: Eval>(input: Pairs<Rule>) -> Result<Expression<T>, ParseError<T>> {
    PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::LITERAL => Ok(Expression::Literal(Spanned::new(
                match T::parse(primary.as_str()) {
                    Ok(v) => v,
                    Err(err) => return Err(ParseError::LiteralParseError(err)),
                },
                primary.as_span().into(),
            ))),
            // Unwrapping is safe here: a group expression always contains exactly 3 pairs.
            Rule::group => parse_expression(primary.into_inner().nth(1).unwrap().into_inner()),
            Rule::expression => parse_expression(primary.into_inner()),
            _ => unreachable!(),
        })
        .map_infix(|left, op, right| {
            Ok(Expression::BinaryOp(Box::new(BinaryOp {
                operand1: left?,
                operand2: right?,
                operator: Spanned::new(
                    match op.as_rule() {
                        Rule::EQ => BinaryOpType::Eq,
                        Rule::NEQ => BinaryOpType::Neq,
                        Rule::GTE => BinaryOpType::Gte,
                        Rule::GT => BinaryOpType::Gt,
                        Rule::LTE => BinaryOpType::Lte,
                        Rule::LT => BinaryOpType::Lt,
                        Rule::AND => BinaryOpType::And,
                        Rule::OR => BinaryOpType::Or,
                        Rule::BIT_AND => BinaryOpType::BitAnd,
                        Rule::BIT_OR => BinaryOpType::BitOr,
                        Rule::ADD => BinaryOpType::Add,
                        Rule::SUB => BinaryOpType::Sub,
                        Rule::MUL => BinaryOpType::Mul,
                        Rule::DIV => BinaryOpType::Div,
                        Rule::MOD => BinaryOpType::Mod,
                        Rule::EXP => BinaryOpType::Exp,
                        _ => unreachable!(),
                    },
                    op.as_span().into(),
                ),
            })))
        })
        .map_prefix(|prefix, expr| {
            Ok(Expression::UnaryOp(Box::new(UnaryOp {
                operand: expr?,
                operator: Spanned::new(
                    match prefix.as_rule() {
                        Rule::PLUS => UnaryOpType::Plus,
                        Rule::MINUS => UnaryOpType::Minus,
                        Rule::NOT => UnaryOpType::Not,
                        Rule::BIT_NOT => UnaryOpType::BitNot,
                        _ => unreachable!(),
                    },
                    prefix.as_span().into(),
                ),
            })))
        })
        .parse(input)
}
