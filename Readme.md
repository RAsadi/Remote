# Remote
Compiler for a simple, C-like language.

# Features

## Expressions

### Unary
* & - Takes the address of a variable 
  `let x = 10; let y = &x;`

# Running
`dune exec remote <path to file>`


# Test
`python3 test/test.py`

Currently, one of the tests fails.

# Dev Notes

## Syscalls
MacOS syscalls are diff than linux, see stdlib/syscallref.h (left out of repo bc license)

## Struct notes
for returning a struct from a function, we can allocate stack space for the struct, and then add a epilogue to the function which copies the stuff out to that allocated stack space, before it destroys itself