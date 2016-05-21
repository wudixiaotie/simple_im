#include <stdio.h>

#ifndef TRUE
#define TRUE 0
#endif

#ifndef FALSE
#define FALSE 1
#endif

#ifndef GET_ARRAY_LEN
#define GET_ARRAY_LEN(array, len) { len = (sizeof(array) / sizeof(array[0])); }
#endif

int main(int argc, char const *argv[])
{
    printf("%s\n", "Hello world!");
    /* defines an array of 10 integers */
    int numbers[10];

    /* populate the array */
    numbers[0] = 10;
    numbers[1] = 20;
    numbers[2] = 30;
    numbers[3] = 40;
    numbers[4] = 50;
    numbers[5] = 60;
    numbers[6] = 70;

    int len;
    GET_ARRAY_LEN(numbers, len)

    for (int i = 0; i < len; ++i)
    {
        /* code */
        printf("%d\n", numbers[i]);
    }

    return 0;
}