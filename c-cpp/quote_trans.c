#include<stdio.h>

int main()
{
#ifdef LOCAL
    freopen("test.in", "r", stdin);
    freopen("test.out", "w", stdout);
#endif

    int c, q = 1;
    while ((c = getchar()) != EOF) {
        if(c == '"') { printf("%s", q ? "“" : "”"); q = !q; }
        else printf("%c", c);
    }
    return 0;
}
