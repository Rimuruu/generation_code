

int main()
{
    long *t;
    t = malloc(8 * 8);
    t[0] = 756;
    printf("%ld", t[0]);
    return 0;
}