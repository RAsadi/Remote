.global __putstr, __malloc
.align 4

// args
//   x0: fd
//   x1: char array
//   x2: array length
// returns
//   nothings?
// clobbers
//   x16
__putstr:
  mov X16, #4
  svc #0x80
  ret


// args
//   x0: amount of bytes to allocate
// returns
//   pointer to allocated memory
// clobbers
//   x8, x16
__malloc:
  // TODO, make this robust from errors, we want to make it return null if it fails
  mov x8, x0
  mov x1, #0
  mov x0, #0xd6
  svc #0x80
  add x1, x8, x0
  svc #0x80
  ret
  // Should check for failure here, if x1 is equal to x0, i think thats a failure