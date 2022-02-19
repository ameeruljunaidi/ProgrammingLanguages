# 3 Things to think about for expressions:

1. Syntax: how we write down the expression
2. Valid types: what types are allowed for subexpressions, and what type this expression returns
3. Evaluation: how we run this when it's part of a program

## Example: Conditionals

Syntax:

- If e1 then e2 then e3
- Where if, the, and else are keywords and
- e1, e2, and e3 are subexpresions

Type checking:

- First e1 must have type bool
- e2 and e3 can have any type t, but must be the same
- Type of entire expression is also t

Evaluation rules:

- First evaluate e1 to a value, v1
- If true, evaluate e2 and that result is the whole expressions's result
- Else, evalute e3 and that result is the whole expression's result

## Exercise: Less Than Comparisons

'val e1 = e2 < e3;'

Syntax:

- If e2 less than e3
- Where < is a keyword
- e2 and e3 are subexpressions

Type checking:

- e2 and e3 are type t, but must be the same type
- e2 and e3 must also be compared to each other
- Type of entire expression will be boolean

Evaluation rules:

- First evaluate e2 < e3, then e1 will take the result of that expressions

# Shadowing

- When the variable already exists in the variable

# 5 Different Thing in Learning

1. Syntax
2. Semantics
3. Idioms
4. Libraries
5. Tools
