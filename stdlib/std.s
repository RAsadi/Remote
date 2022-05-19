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
  mov x1, x0 // Number of bytes to allocate
  // This is a cursed mmap syscall thing
  mov x0, #0 // Address ptr, set to null
  mov x2, #0x3 // protection flags, set to READ | WRITE
  mov x3, #0x1002 // mapping flags, set to MAP_ANON | MAP_PRIVATE
  mov x4, #0 // fd, 0 because we are using anonymous mapping
  mov x5, #0 // offset into file, zero for the same reason as above
  mov x16, #197
  svc #0x80
  ret