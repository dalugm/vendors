#include <stdio.h>

int main() {

    double x[3], z[3], x_[3], z_[3];
    double k[3], b[3];
    double Cx[3], Cz[3];
    double CW, CH;

    while (1) {
        for (int i = 0; i < 3; i++) {
            printf("输入第 %d 次扔末影之眼时的 x 坐标以及 z 坐标：", i + 1);
            scanf("%lf %lf", &x[i], &z[i]);
            printf("输入第 %d 次末影之眼落地的 x 坐标以及 z 坐标：", i + 1);
            scanf("%lf %lf", &x_[i], &z_[i]);
            printf("\n");
        }

        for (int i = 0; i < 3; i++) {
            k[i] = (z_[i] - z[i]) / (x_[i] - x[i]);
            b[i] = z[i] - (k[i] * x[i]);
        }

        for (int i = 0; i < 2; i++) {
            Cx[i] = (b[i] - b[i+1]) / (k[i+1] - k[i]);
            Cz[i] = k[i] * Cx[i] + b[i];
        }

        Cx[2] = (b[0] - b[2]) / (k[2] - k[0]);
        Cz[2] = k[0] * Cx[2] + b[0];

        double x_max, x_min, z_max, z_min;

        x_max = x_min = Cx[0];
        if (Cx[1] < x_min) x_min = Cx[1];
        if (Cx[2] < x_min) x_min = Cx[2];

        if (Cx[1] > x_max) x_max = Cx[1];
        if (Cx[2] > x_max) x_max = Cx[2];

        z_max = z_min = Cz[0];
        if (Cz[1] < z_min) z_min = Cz[1];
        if (Cz[2] < z_min) z_min = Cz[2];

        if (Cz[1] > z_max) z_max = Cz[1];
        if (Cz[2] > z_max) z_max = Cz[2];

        CW = 0.5 * (x_min + x_max);
        CH = 0.5 * (z_min + z_max);

        printf("要塞坐标位于（%lf, %lf）\n\n", CW, CH);
        printf("----------------调试信息-----------------\n");
        printf("确认输入：第一次 (%.2lf, %.2lf)  (%.2lf, %.2lf)\n",
               x[0], z[0], x_[0], z_[0]);
        printf("确认输入：第二次 (%.2lf, %.2lf)  (%.2lf, %.2lf)\n",
               x[1], z[1], x_[1], z_[1]);
        printf("确认输入：第三次 (%.2lf, %.2lf)  (%.2lf, %.2lf)\n",
               x[2], z[2], x_[2], z_[2]);
        printf("三条解析方程为：z=%.2lfx%+.2lf  z=%.2lfx%+.2lf  z=%.2lfx%+.2lf\n",
               k[0], b[0], k[1], b[1], k[2], b[2]);
        printf("确认交点：(%.2lf, %.2lf) (%.2lf, %.2lf) (%.2lf, %.2lf)\n\n",
               Cx[0], Cz[0], Cx[1], Cz[1], Cx[2], Cz[2]);
    }

    return 0;
}
