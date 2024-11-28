# Interpreter

## Notes

- [ ] Fix assignment lhs to allow more complex expressions
- [ ] Decide on call semantics
- [ ] Floats and negative numbers
- [ ] Fixed percision numbers
- [ ] Make strings more concrete
- [ ] Variable type inference
- [ ] Decide on built in data structures
- [ ] Basic standard library

## Stages

1. Tokenising
2. Parsing
3. Typechecking
4. Evaluation

## Features

### Procedures

```
add(x int, y int) int {
    return x + y;
}

main() unit {
    print(add(1, 2));
}
```

### Records

```
Point2D record {
    x int,
    y int
}

main() unit {
    let p1 Point = {
        x = 1,
        y = 2
    };

    print(p1.x);
}
```

### Variables

```
main() unit {
    let x int = add(1, 2);
    print(x);
}
```

### Branching

```
if x > 5 {
    print("greater than 5");
} else {
    print("less than 5");
}

```

### Loops

```
while x < 10 {
    print(x);
    x = x + 1;
}

for i in 0..10 {
    print(i);
}
```

### Arrays

```
sum(xs [int]) int {
    let sum int = 0;

    for i in 0..xs.len {
        sum = sum + xs[i];
    }

    return sum;
}


main() unit {
    let numbers [int] = [1, 2, 3, 4, 5];
    print(sum(numbers));
}
```
