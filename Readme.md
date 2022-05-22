# Remote

Compiler for a simple, C-like language. Created because I wanted to write a C compiler for fun, but the grammar of C got to be a bit too weird at some point.

So, I designed a language that behaves a lot like C, but with an easier to understand, LR(1) grammar

# Running

`dune exec remote <path to file.rc>`

Then, the compiled file will be in `build/<filename>`

# Test

`python3 test/test.py`

Currently, one of the tests fails, due to a planned but unimplemented feature

# Demo

A series of sample remote programs exist in `demo/`

Here are a few of them

### Fibonacci

#### Code

```
fn fibonacci(n: u32) -> u32 {
  if (n == 0) {
    return 0;
  } else if (n == 1) {
    return 1;
  } else {
    return fibonacci(n - 1) + fibonacci(n - 2);
  }
}

fn main() -> u32 {
  print_number(fibonacci(10));
  return 0;
}
```

#### Output
```
55
```

### Is Prime

#### Code

```
fn is_prime(n: u32) -> bool {
  if (n == 1) {
    return false;
  }

  for (let mut i = 2; i < n; i++) {
    if (n % i == 0) {
      return false;
    }
  }

  return true;
}

fn main() -> u32 {
  let num_to_check = 273;

  print_number(num_to_check);

  if (is_prime(num_to_check)) {
    print(" is prime\n", 10);
  } else {
    print(" is not prime\n", 14);
  }

  return 0;
}
```

#### Output
```
273 is not prime
```

### sum_linked_list

#### Code
```
struct node {
  next: node^,
  item: u32
}

fn sum_linked_list(n: node^) -> u32 {
  if (n == null) {
    return 0;
  }

  return (n^).item + sum_linked_list((n^).next);
}

fn main() -> u32 {

  let first_node = node{null, 1};
  let second_node = node{&first_node, 2};
  let third_node = node{&second_node, 3};

  let num = sum_linked_list(&third_node);

  print_number(num);
  print("\n", 1);

  return 0;
}
```

#### Output
```
5
```

# Full feature description

## Expressions

### Unary

- & - Takes the address of a variable

  `let x = 10; let y = &x;`

- ! - Logical not

  `let truthy = !false;`

- \- - Negate

  `let negative = -10;`

  Note, currently doesn't work since there is no signed number type

- ~ - Bitwise not

  `let bitnot = ~10`;

### Binary

#### Arithmetic

- \+ - Add

  `let sum = 10 + 2;`

- \- - Subtract

  `let diff = 10 - 2;`

- \* - Multiply

  `let product = 10 * 2;`

- / - Divide

  `let div = 20 / 10;`

- % - Modulo

  `let mod = 20 % 10;`

#### Bitwise

- << - Left shift

  `let times2 = 10 << 1;`

- \>> - Right shift

  `let div2 = 10 >> 1;`

- & - And

  `let bitwise_and = 10 & 2;`

- | - Or

  `let bitwise_or = 10 | 2;`

- ^ - Exclusive Or

  `let bitwise_xor = 10 ^ 2;`

#### Logical

- || - Or

  `let logical_or = true || false;`

- && - And

  `let logical_and = true && false;`

#### Comparison

- == - Equal

  `let eq = 1 == 1;`

- != - Not equal

  `let neq = 1 != 1;`

- < - Less than

  `let lt = 1 < 2;`

- <= - Less than or equal to

  `let lt = 1 <= 2;`

- \> - Greater than

  `let lt = 1 > 2;`

- \>= - Greater than or equal to

  `let lt = 1 >= 2;`

# Dev Notes

## Syscalls

MacOS syscalls are diff than linux, see stdlib/syscallref.h (left out of repo bc license)

## Struct notes

for returning a struct from a function, we can allocate stack space for the struct, and then add a epilogue to the function which copies the stuff out to that allocated stack space, before it destroys itself
