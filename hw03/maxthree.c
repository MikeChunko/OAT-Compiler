#include <stdio.h>

int max (int a, int b, int c) {
    if (a >= b && a >= c)
        return a;
    else if (b >= a && b>= c)
        return b;
    else
        return c;
}

int main (){
    int a1 = 11;
    int a2 = 22; 
    int a3 = 26;
    return max (a1, a2, a3);
}
