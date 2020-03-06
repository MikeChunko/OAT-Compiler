#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

int64_t ll_atoi(int8_t* str){
    int64_t x = 0L, mul = 1L;

    // Iterate through all characters of input string and 
    // update result 
    for (int i = 0; str[i] != '\0'; ++i) {
        if (i == 0 && str[i] == '-')
            mul = -1L;
        else x = x * 10L + str[i] - '0'; 
    }
  
    // return result.
    return x * mul; 

}

void ll_printint(int64_t x){
    printf("%ld", x);
}

void ll_puts(int8_t *s) {
  puts((char *)s);
}

int8_t* ll_ltoa(int64_t i) {
  char* buf = (char*)calloc(20, sizeof(char));
  snprintf((char *)buf, 20, "%ld", (long)i);
  return (int8_t *)buf;
}