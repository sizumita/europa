#include <stdio.h>
#include <string.h>

extern int printi(long X) {
  fprintf(stderr, "%ld\n", X);
  return 0;
}

extern long read_int() {
    long dt;
    scanf("%ld", &dt);
    return dt;
}

extern int println(char *sp) {
  printf("%s\n", sp);
  return 0;
}

extern long __String__compare(char *sp, char *sp2) {
  return strcmp(sp, sp2);
}
