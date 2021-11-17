#include <stdio.h>

/* putchard - putchar that take a double and returns 0. */
extern double putchard(double X) {
  putc((char)X, stderr);
  return 0;
}

/* printd - printf that takes a double prints it as "%f\n", returning 0. */
extern int printi(long X) {
  fprintf(stderr, "%ld\n", X);
  return 0;
}
