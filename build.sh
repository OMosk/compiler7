#!/bin/bash

set -e
set -x
rm -f compiler

CFLAGS="-masm=intel -nodefaultlibs -ggdb -O0 -Wall -Wextra -pthread -Wno-unused-parameter -fno-omit-frame-pointer -Werror=switch"
CXXFLAGS="$CFLAGS -fno-rtti -fno-exceptions"
LDFLAGS="-lc"

time clang $CFLAGS tools/stdin2array.c -o tools/stdin2array $LDFLAGS

# -fno-stack-protector is needed to prevent clang from instrumenting our code
# with calls that checks stack. These functions are provided by libc and do not
# want to link with it
time clang $CFLAGS -ffreestanding -fno-stack-protector -c support_library/c7.c -o - | ./tools/stdin2array support_library_o > src/generated_support_library_o.cpp

time clang++ $CXXFLAGS src/all.cpp -o compiler $LDFLAGS
