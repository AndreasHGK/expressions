# Expressions

A flexible expression parser and evaluator crate. Custom types in expressions are supported through a trait.

```rust
assert_eq!(expressions::eval::<i32>("2 ^ (3) ^ 1 + 4 - 3 * -2"), Ok(18));
```

## Supported types
By default, the `Eval` trait is implemented on all integers and all floats. If these default implementations are not 
desired, a new implementation can be made through a wrapper type.
Support for custom types can also be added by implementing `Eval` for this type.

The default implementations use lazy evaluation for the boolean operators. Bitwise operations are not supported for 
floats, and will return an error when evaluated. Integers will return an error when there is overflow or a division by 
zero.

## Operators
The following operators are currently supported:

#### Comparison operators
 - `==` **Equals**
 - `!=` **Not equals**
 - `>=` **Greater than or equals**
 - `>` **Greater than**
 - `<=` **Less than or equals**
 - `<` **Less than**

#### Boolean operators
 - `&&` **Boolean AND**
 - `||` **Boolean OR**
 - `!` **Boolean NOT** (unary)

#### Bitwise operators
 - `&` **Bitwise AND**
 - `|` **Bitwise OR**
 - `~` **Bitwise NOT** (unary)

#### Arithmetic operators
 - `+` **Addition**
 - `-` **Subtraction**
 - `*` **Multiplication**
 - `/` **Division**
 - `%` **Modulo**
 - `^` **Exponentiation**
 - `+` **Plus** (unary)
 - `-` **Minus** (unary)

## Examples
 - [complex](./examples/complex.rs) - A basic complex number calculator. Shows how to evaluate expressions with your
 own types.
