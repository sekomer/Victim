# Victim

Victim is dynamically typed interpreted scripting language written in `Haskell`. The name is inspired by source code of [malloc](https://code.woboq.org/userspace/glibc/malloc/malloc.c.html#3038).

## Installation

You need `Glasgow Haskell Compiler` and `Cabal` to install `Victim` interpreter on your computer.

```bash
cabal install -O2 --overwrite-policy=always 
```
creates `Victim` symlink sto original binary.

## Usage
To run `Victim` interpreter.
```sh
Victim main.v
```

## Data Types

| name     | description                                |
| -------- | ------------------------------------------ |
| Integer  | Whole number                               | 
| Double   | Number with floating points                |
| Bool     | Truth values, internally Integer 0 and 1   |
| String   | Sequence of characters                     |
| Function | Subroutine                                 |
| Null     | Representation of uselessness, inspired by the [legend](https://en.wikipedia.org/wiki/JavaScript) |

## Examples
Examples are stored under [example]() folder.

## Language

### Comments
```Haskell
-- this is a single line comment

{-
 this
 is
 a
 multi
 line
 comment
-}
```
### Variable Decleration and Assign
`var` keyword is used to declare a variable.
```csharp
var num := null;    -- declare
num := 42           -- assign
```

### Control Flow
```python
-- single line conditional statements doesnt require braces
var cond := true;
if (cond) print( "yeey" );


-- multi line conditional statements require braces
var a := 2;
var b := 3;
var op := "add";

if ( op == "add" )
{
    var res := a + b;
    print(res);
}
else if ( op == "mul" )
{
    var res := a * b;
    print(res);
}
else    print( "unknown operation!" );

```

## Loops
`while` keyword is used to create a loop statement.
```cs
var condition := true;

while (condition)
{
    print( "YES!" );
}
```
loops support both `continue` and `break` statements.

## Functions

### [ Named Functions ] 

Named functions can be created with `fn` keyword. 
```rust
fn add(a, b)
{
    return a + b;
}
```
Functions are not required to have a return expression. `return` statement without an `expression` and functions without `return` statements return `null`.

### [ Anonymous Functions ]
`Anonymous` functions can be created with the `anon` keyword. Anonymous functions are expressions, therefore, they need to be assigned to a variable or passed into the function as a parameter.

```rust
-- passed
fn apply (f, n)
{
    return f(n);
}

print( apply(anon x -> x**2, 5) );
```

```rust
-- assigned
var f := anon x -> x**2;
print( f(5) );
```

## Contributing
Please open an issue to discuss what you would like to change.


## License
[MIT](https://choosealicense.com/licenses/mit/)