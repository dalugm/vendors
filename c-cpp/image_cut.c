#include <stdio.h>

int main()
{
    float input_width = 0, input_height = 0;
    float dest_width, dest_height;
    float width, height;
    int int_w, int_h;

    printf("Target width and height (e.g., 156 210): ");
    scanf("%f %f", &dest_width, &dest_height);

    while(1) {
        printf("\nCurrent width (enter 0 to skip): ");
        scanf("%f", &input_width);

        if (input_width == 0) {
            printf("\nCurrent height (enter 0 to quit): ");
            scanf("%f", &input_height);
        }

        if (input_width == 0 && input_height == 0) {
            break;
        }

        if (input_width) {
            width = input_width;
            height = width / dest_width * dest_height;
        }

        if (input_height) {
            height = input_height;
            width = height / dest_height * dest_width;
        }

        int_w = width + 0.5;
        int_h = height + 0.5;

        printf("\nWidth: %d, Height: %d.\n", int_w, int_h);
        input_width = 0, input_height = 0;
    }

    return 0;
}
