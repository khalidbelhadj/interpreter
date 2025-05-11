# Interpreter

## Notes

- [x] Fix assignment lhs to allow more complex expressions
    - done by considering lvalue and rvalue expressions
- [x] Decide on call semantics
    - call by value, will introduce references later
- [x] Decide on built in data structures
    - Only arrays, will add a standard library with more later
- [x] Actually implement primitive function `#`
- [x] Slices
    - question mark syntax for slices
- [ ] Type checking of branches
- [ ] Dynamic memory allocation
    - `#allocate` primitive
- [ ] `not` operator
- [ ] Floats and negative numbers
- [ ] Fixed percision numbers
- [ ] Make strings more concrete, array of chars? How do we represent them?
- [ ] Type inference

## Stages

1. Tokenising
2. Parsing
3. Typechecking
4. Evaluation

## Features

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

### Structs

Similar to procedures, structs are defined using the `::` operator and the struct keyword. In braces, the fields are defined with their types. To instantiate a struct, the struct literal syntax is used, which is similar to the struct definition. The fields are defined with their values after `=`. Fields can be accessed using the dot operator.

```
Point2D :: struct {
    x int,
    y int
}

main :: () unit {
    let p1 Point = {
        x = 1,
        y = 2
    };

    #print(p1.x);
}
```

### Variables
Variables are defined using the `let` keyword. The type of the variable is specified after the variable name. At this point in time the type is required, but in the future it will be inferred.

```
main :: () unit {
    let x int = add(1, 2);
    #print(x);
}
```

### Branching

Braching is done using the `if` and `else` keywords. the elif keyword is not supported at this time.

```
if x > 5 {
    #print("greater than 5");
} else {
    #print("less than 5");
}

```

### Loops

Loops are done using the `while` and `for` keywords. The `for` keyword is used to iterate over a range of numbers. Iterators would be cool in the future.

```
while x < 10 {
    #print(x);
    x = x + 1;
}

for i in 0..10 {
    #print(i);
}
```

### Arrays

Arrays are the main primitive for contiguous memory. They are defined using `[]` syntax. At the moment the type requires a size parameter, dynamic arrays are inn the works.

```
sum :: (xs [int, 5]) int {
    let sum int = 0;

    for i in 0..#length(xs) {
        sum = sum + xs[i];
    }

    return sum;
}


main :: () unit {
    let numbers [int, 5] = [1, 2, 3, 4, 5];
    #print(sum(numbers));
}
```

The `#length` builtin is used to get the length of an array.

### Slices

Slices are similar to arrays, but they do not require a size parameter. They are defined using `?` syntax. Slices are passed by reference, so they can be modified in place.

```

double :: (xs [int, ?]) unit {
    for i in 0..#length(xs) {
        xs[i] = xs[i] * 2;
    }
}

main :: () unit {
    let numbers [int, ?] = [1, 2, 3, 4, 5];
    #print(numbers);
    double(numbers);
    #print(numbers);
}
```

The `#length` builtin is used to get the length of an slice.
