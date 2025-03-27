/* fib.c */

int scanf(char *s, ...);
int printf(char *s, ...);

/*
 * return the nth fibonacci number
 */

int fib(int n)
{
    if (n == 0 || n == 1) return n;
    return fib(n - 1) + fib(n - 2);
}


int main(void)
{
    int n;

    scanf("%d", &n);
    printf("%d\n", fib(n));
}
