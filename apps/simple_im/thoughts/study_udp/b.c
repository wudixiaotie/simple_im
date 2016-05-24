#include <stdio.h>

int main()
{
    int input[2];
    for (int i = 0; i < 2; ++i)
    {
        scanf("%d", &input[i]);
    }

    int sum = 0;
    for (int i = 0; i < 2; ++i)
    {
        sum += input[i];
    }

    printf("%d\n", sum);

    return 0;
}