#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

void prnbool(int64_t val) {
  if(val) {
    printf("true\n");
  } else {
    printf("false\n");
  }
}

void prnint(int64_t val) {
  printf("%lld\n",val);
}

void prnstr(char *val) {
  printf("%s\n",val);
}

void prnintn(int64_t* val) {
  printf("[");
  int length = val[0];
  for(int i=0;i!=length;++i) {
    if(i != 0) {
      printf(", ");
    }
    printf("%lld",val[i+1]);
  }
  printf("]\n");  
}

/**
 * Compare two integer arrays for equality
 */
int intncmp(const int64_t* arr1, const int64_t *arr2, int64_t n) {
  for(int i=0;i<n;++i) {
    if(arr1[i] != arr2[i]) {
      return 0; // false
    }
  }
  return 1; // true
}

/**
 * Copy one integer array into another.  Both arrays are assumed to
 * have the same size.
 */
int64_t *intncpy(int64_t* dest, const int64_t *src, int64_t n) {
  for(int i=0;i<n;++i) {
    dest[i] = src[i];
  }
  return dest;
}

/**
 * Fill a given array with a single value
 */
int64_t *intnfill(int64_t* dest, int64_t n, int64_t v) {
  for(int i=0;i<n;++i) {
    dest[i] = v;
  }
  return dest;
}

/**
 * Implement an assertion in WhileLang as a C assert.  Unfortunately, 
 * we cannot call "assert" directly as a function because it is, in fact, a macro.
 */
void assertion(int64_t boolean) {
  assert(boolean);
}



