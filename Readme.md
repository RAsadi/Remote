# Running
`dune exec remote <path to file>`

# Test
`python3 test/test.py`

# Notes

## Syscalls
See https://chromium.googlesource.com/chromiumos/docs/+/master/constants/syscalls.md#arm64-64_bit

## Array notes
put this in the stdlib

struct array {
  ptr: u32*,
  size: u32,
}

then

`let a = [5, 10, 13];`

should just be desugared to something like 

```
let _a_arr_ptr = __malloc(3 * sizeof(array_member));
a^ = 5;
(a + 1)^ = 10;
(a + 2)^ = 13;
let a = array{
  ptr = arr_ptr,
  size = 3
};
```

## Struct notes
for returning a struct from a function, we can allocate stack space for the struct, and then add a epilogue to the function which copies the stuff out to that allocated stack space, before it destroys itself