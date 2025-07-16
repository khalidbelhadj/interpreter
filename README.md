# Interpreter

This is an interpreter for a made up language. The purpose of this project is to learn more about programming languages by implementing parsing, type checking, code generation, garbage collection and a virtual machine.

Parsing is done using a recursive decent approach, but there is no specified formal grammar I'm kind of just winging it. The syntax of the language may change in the future, but the general idea is to stick with simple expressive syntax with no weird expressions or whatever. Something similar to the typical procedural languages.

Type checking uses a bidirectional type checker. The first mode is inference mode which takes an expression and inferes the type from it. The second mode is checking, which takes and expression and a type and ensures that the expressions is the expected type. This gives us flexability to do more advances type inference in the future. Otherwise, the type system is nominal, with strong explicit types.

There is no target of compilation yet, the AST is evaluated directly with a simple VM. The idea is that the language will be compiled to bytecode which runs on it's own VM. At the moment there is no consideration for garbage collection and memory is reference counted, but I plan to implement a garbage collector for the language.

## Language Reference

### Procedures

Procedures are defined using the `::` operator denoting a compile time constant. The required return type is specified after the parameters. All variables passed into a procedure are passed by value.

```
add :: (x int, y int) int {
    return x + y;
}

main :: () unit {
    #print(add(1, 2));
}
```

Some procedures are build in to the language and are prefixed with `#` such as `#print` and `#length`.

### Variables

Variables are defined using the `let` keyword. The type of the variable is specified after the variable name. At this point in time the type is required, but in the future it will be inferred.

```
let x int = add(1, 2);
#print(x);

// Literals
let i int = 1;
let f float = 1.5;
let s string = "string literal";
```

### Structs

Similar to procedures, structs are defined using the `::` operator and the struct keyword. In braces, the fields are defined with their types. To instantiate a struct, the struct literal syntax is used, which is similar to the struct definition. The fields are defined with their values after `=`. Fields can be accessed using the dot operator.

```
// Declaring a struct
Point2D :: struct {
    x int,
    y int
}

// Using the struct
let p1 Point = Point {
    x = 1,
    y = 2
};

#print(p1.x);
```

### Logic

Logical expressions use the `not`, `or` and `and` operator to denote negation, disjunction and conjunction respecively.

```
let a bool = false;
let b bool = my_proc();
let c bool = true;
let d bool = not a and b or c
```

### Branching

Braching is done using the `if` and `else` keywords. `else if` is not supported at this time.

```
if x > 5 {
    #print("greater than 5");
} else {
    #print("less than 5");
}

```

### Loops

Loops are done using the `while` and `for` keywords. The `for` keyword is used to iterate over a range of numbers. Use the `#range` directive to iterate over a specified range. If a single argument is provided, the iteration goes from 0 to the value, if two are provided then it goes from the first to the second.

```
while x < 10 {
    #print(x);
    x = x + 1;
}

for i in #range(0, 10) {
    #print(i);
}

for i in #range(10) {
    #print(i);
}
```

### Arrays and Slices

Arrays are the main primitive for contiguous memory. They are defined using `[]` syntax, with a size parameter inside the brackets and a following element type. Arrays have a predetermined size which is provided at compile time. Arrays can be instantitated using the array literal syntax `[1, 2, 3]`.

Slices are similar to arrays, but their size is determined at runtime. Conceptually, a slice is simply it's header
```
SliceHeader :: struct {
    length int,
    data &T
}
```

Slices can both be indexed, and are passed by value. When a slice is passed by value, it's elements can be modified. An array passed as a copy of all the elements.


```
sum_array :: (xs [5]int) int {
    let sum int = 0;
    for i in #range(5) {
        sum = sum + xs[i];
    }
    return sum;
}

sum_slice :: (xs []int) int {
    let sum int = 0;
    for i in #range(5) {
        sum = sum + xs[i];
    }
    return sum;
}


let xs [5]int = [1, 2, 3, 4, 5];
sum_array(xs)
let ys []int = [1, 2, 3, 4, 5];
sum_slice(ys)
```

The `#length` builtin is used to get the length of an array or slice. If the length of an array is not known at compile time, it can be created using the `#array` directive.

```
let length int = 20;
let xs []int = #array(int, 20)
```

This creates a slice of the specified type and size with it's default value.

### Default Values

In some cases, variables might be uninitialised. In this case they take their default value. Primitive types have specified default values, and structs have fields which recursively take their default values.

| Type | Value |
| --- | ---
| `int` | `0` |
| `float` | `0.0` |
| `string` | `""` |
| `[]T` | `[]` |

## References

References are memory addresses of values on the stack or heap.

```
pass_by_value :: (x int) unit {
    x = 20;
}

pass_by_ref :: (r &int) unit {
    *r = 20;
}

let x int = 10;
let r &int = &x;
pass_by_value(x);
// x is still 10
pass_by_ref(r);
// x is now 20
```

## Comments
Comments are C-like, `//` for single line and `/**/` for multi line. The language supports nested comments too.

```
// This is a line comment
/*
This is a multi line comment
    /*
    This is a nested multiline comment
    */
*/
```

## To do

- [x] Fix assignment lhs to allow more complex expressions
    - done by considering lvalue and rvalue expressions
- [x] Decide on call semantics
    - call by value, will introduce references later
- [x] Decide on built in data structures
    - Only arrays, will add a standard library with more later
- [x] Actually implement primitive function `#`
- [x] Slices
    - question mark syntax for slices
- [x] Type checking of branches
- [x] Location errors
- [x] Combinding unary operators like `[]` and `.`
- [x] Reference semantics
  - use the `&` operator to get the reference of an expression
  - use references as types and function semanticsn
- [x] Floats
- [x] `not` operator
- [x] `#array` directive for dynamically allocated array
- [x] Default values
    - Figure out what the default value of things are
    - Assign default value for struct fields
- [x] Allow identifiers that start with underscore
- [x] Make fields optional in struct fields
    - This was a bit sketchy, maybe go over how types are passed to the evaluator. Might be useful in the future for other things.
- [x] Negative numbers
- [ ] Start implementing test cases
- [ ] Stack trace on panic in evaluation
    - Distinguish between user and compiler errors
- [ ] Fix spans being more accurate
    - Remove Span::empty
- [ ] Make strings more concrete
    - array of chars? How do we represent them?
    - multiline, special chars, utf-8 or whatever
- [ ] Const expressions
- [ ] Variants
    - Enums
    - Raw unions
    - Tagged union
- [ ] Better resolving of top levels, out of order
- [ ] Recursive types
- [ ] Type casting
- [ ] Fixed precision numbers like u32, i64 and so on
- [ ] Tuples
- [ ] Generics
- [ ] Type inference
- [ ] Standard library
    - collections
    - io
    - maths
    - os
    - date/time
    - libc like library

