# Expressions

A flexible expression parser and evaluator crate. Custom types in expressions are supported through a trait.

```rust
assert_eq!(expressions::eval::<i32>("2 ^ (3) ^ 1 + 4 - 3 * -2"), Ok(18));
```

## Examples
 - [complex](./examples/complex.rs) - A basic complex number calculator. Shows how to evaluate expressions with your
 own types.
