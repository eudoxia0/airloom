# Hello, World in C

Traditionally, the way to kick the tires on a programming language is to
write a program that simply prints "Hello, world!" and exits. In C, the
function for printing text is called `printf`, and we use it like this:

```c
printf("Hello, World!\n");
```

The whole program looks like this:

```c
#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    printf("Hello, World!\n");
    return EXIT_SUCCESS;
}
```