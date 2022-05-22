# 🎮 Remote

Remote is a simple, C-like language, created because I wanted to write a C compiler for fun, but the grammar of C got a bit too weird at some point.

So, I designed a language that behaves a lot like C, but with an easier to understand, LALR(1) grammar

# 🏃‍♂️ Running

`dune exec remote <path to file.rc>`

Then, the compiled file will be in `build/<filename>`

# 🧪 Test

`python3 test/test.py`

Currently, one of the tests fails, due to a planned but unimplemented feature

# 📝 Demo

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

### Linked list

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

# 💼 Full feature description

## Types

Remote currently supports the following types

| Type        | Description                                                     |
| ----------- | --------------------------------------------------------------- |
| u8          | An 8-bit, unsigned integer                                      |
| u32         | A 32-bit, unsigned integer                                      |
| bool        | A boolean value, either true or false                           |
| t^          | A pointer to a type t. You can take a pointer of any other type |
| struct_name | A struct type. See below for more information                   |

### Structs

#### Description

A struct is a C-like struct, essentially a user defined data type containing other types.

#### Example

```
struct node {
  data: u32,
  next: node^
}

fn main() {
  let example_node = node{10, null};
  // example_node.next is null, and example.data is 10
}
```

#### Notes

Structs currently cannot be returned from functions.

## Variables

### Description

Remote has support for variables. Currently, all variables are immutable by default, but can be
made mutable by the `mut` keyword. Types for variables can be inferred by their initialier, or can be explictly annotated. Variables are bound to their block scope, like in C.

### Example

```

fn main() -> u32 {
  let v1 = 10;            // v1 has type u8, as the literal 10 has type u8
  let v2: u32 = 10;       // v2 has type u32, since the literal 10 can be implicitly converted to u32
  let mut v3: u32 = 10;  // v3 is a mutable u32
  v3 = v2 + 1;           // v3 is now 11
  v2 = v3;               // Will error, since v2 is immutable
}
```

## Functions

### Description

Remote has support for functions, declared with the `fn` keyword. They are not first class values in Remote, and can only be called right now.

The main function, which returns a u32, is the entry point for the program.

### Example

```
// f1 is a function which takes in a u32, and a pointer to a u32, and returns a boolean value
fn f1(x: u32, z: u32^) -> bool {

}

// f1 can be called like:
// let x = 10;
// let res: bool = f1(10, &x);
```

## Statements

### If

#### Description

Exactly like a C-style if statement.

#### Example

```
if (cond1 || cond2) {
  do_something();
} else if (cond3) {
  do_another_thing();
} else {
  do_a_third_thing();
}
```

### While

#### Description

Exactly like a C-style while loop

#### Example

```
while(cond1) {
  if (cond2) {
    continue;
  }
  do_something();
  break;
}
```

### For

#### Description

Exactly like a C-style for loop, but must have a declarator, condition, and post expression

#### Example

```
for(let mut i = 0; i < 10; i++) {
  if (i % 2 == 0) {
    continue;
  }
  do_something(i);
}
```

## Expressions

### Literals

| Literal   | Description                                                                | Example                           |
| --------- | -------------------------------------------------------------------------- | --------------------------------- |
| Character | Character literal, has type `u8`                                           | `let ch = 'a';`                   |
| String    | String literal, has type `u8^`                                             | `let str = "hello";`              |
| Number    | Number literal, has type `u8`, or `u32` if the literal won't fit in a `u8` | `let n = 1024;`                   |
| Bool      | Boolean literal, has type bool                                             | `let yes = true; let no = false;` |

### Unary

| Operator | Description                     | Example                   |
| -------- | ------------------------------- | ------------------------- |
| &        | Takes the address of a variable | `let x = 10; let y = &x;` |
| \|       | Logical not                     | `let truthy = !false;`    |
| -        | Negate                          | `let negative = -10;`     |
| ~        | Bitwise not                     | `let bitnot = ~10;`       |

### Binary

#### Arithmetic

| Operator | Description | Example                 |
| -------- | ----------- | ----------------------- |
| \+       | Add         | `let sum = 10 + 2;`     |
| \|       | Subtract    | `let diff = 10 - 2;`    |
| \*       | Multiply    | `let product = 10 * 2;` |
| /        | Divide      | `let div = 20 / 10;`    |
| %        | Modulo      | `let mod = 20 % 10;`    |

#### Bitwise

| Operator | Description  | Example                     |
| -------- | ------------ | --------------------------- |
| <<       | Left shift   | `let times2 = 10 << 1;`     |
| \>>      | Right shift  | `let div2 = 10 >> 1;`       |
| &        | And          | `let bitwise_and = 10 & 2;` |
| \|       | Or           | `let bitwise_or = 10 \| 2;` |
| ^        | Exclusive Or | `let bitwise_xor = 10 ^ 2;` |

#### Logical

| Operator | Description | Example                             |
| -------- | ----------- | ----------------------------------- |
| \|\|     | Or          | `let logical_or = true \|\| false;` |
| &&       | And         | `let logical_and = true && false;`  |

#### Comparison

| Operator | Description              | Example             |
| -------- | ------------------------ | ------------------- |
| ==       | Equal                    | `let eq = 1 == 1;`  |
| !=       | Not equal                | `let neq = 1 != 1;` |
| <        | Less than                | `let lt = 1 < 2;`   |
| <=       | Less than or equal to    | `let lte = 1 <= 2;` |
| \>       | Greater than             | `let gt = 1 > 2;`   |
| \>       | Greater than or equal to | `let gte = 1 >= 2;` |

### Postfix

| Operator | Description | Example                              |
| -------- | ----------- | ------------------------------------ |
| ++       | Increment   | `let x = 10; x++;`                   |
| --       | Decrement   | `let y = 10; y--;`                   |
| ^        | Dereference | `let x = 1; let y = &x; let z = y^;` |

### Sizeof

#### Description

A builtin function which returns the size of an expression or type.

#### Example

```
struct example_struct {
  member1: u32,
  member2: u8
}

fn main() -> u32 {
  let x = sizeof(example_struct); // x should be 5, since example struct is 5 bytes in size
  let y = sizeof(x); // y should be 4, since sizeof returns a u32, so x is 4 bytes in size
}
```

### Cast

#### Description

Casts one type to another, currently functions like a `reinterpret_cast` in c++.
Note that parens are required around a cast expression, in order to avoid a parsing issue.

#### Example

```
fn takes_u8(i: u8) {}

fn main() -> u32 {
  let x: u32 = 10;
  takes_u8((x as u32));
}
```

# Dev Notes

## Syscalls

MacOS syscalls are diff than linux, see stdlib/syscallref.h (left out of repo bc license)

## Struct notes

for returning a struct from a function, we can allocate stack space for the struct, and then add a epilogue to the function which copies the stuff out to that allocated stack space, before it destroys itself
