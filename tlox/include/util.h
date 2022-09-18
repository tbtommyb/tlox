#ifndef UTIL_H_
#define UTIL_H_

#define OPTIONAL(type)                                                         \
  struct {                                                                     \
    bool present;                                                              \
    type value;                                                                \
  }
#define OPTIONAL_HAS_VALUE(opt) ((opt).present)
#define OPTIONAL_VALUE(opt) ((opt).value)
#endif // UTIL_H_
