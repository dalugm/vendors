#include <stdio.h>

int main()
{
    int shour, smin, ssec;
    int ehour, emin, esec;
    int seconds;

    printf("input the start time: ");
    scanf("%d %d %d", &shour, &smin, &ssec);

    printf("input the end time: ");
    scanf("%d %d %d", &ehour, &emin, &esec);

    if (emin < smin) {
        if (esec < ssec) {
            seconds = (ehour - shour - 1) * 3600 + (emin + 60 - smin - 1) * 60 + (esec + 60 - ssec);
            printf("The total seconds is: %d\n", seconds);
        }
        else {
            seconds = (ehour - shour - 1) * 3600 + (emin + 60 - smin - 1) * 60 + (esec + 60 - ssec);
            printf("The total seconds is: %d\n", seconds);
        }
    }
    else {
        if (esec < ssec) {
            seconds = (ehour - shour) * 3600 + (emin - smin) * 60 + (esec - ssec);
            printf("The total seconds is: %d\n", seconds);
        }
        else {
            seconds = (ehour - shour) * 3600 + (emin - smin) * 60 + (esec - ssec);
            printf("The total seconds is: %d\n", seconds);
        }
    }

    return 0;
}
