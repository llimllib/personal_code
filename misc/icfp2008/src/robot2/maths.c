
#include "math.h"
#include "maths.h"

#include <stdio.h>

double get_distance(double x1, double y1, double x2, double y2) {
    return sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1));
}

double get_angle(double x1, double y1, double x2, double y2) {
    double angle;
    double x = x2 - x1;
    double y = y2 - y1;
    
    if (fabs(x) > fabs(y)) {
        
        angle = to_deg(atan(y / x));
        
        if (x > 0 && y > 0) {
            // do nothing
        } else if (x < 0) {
            angle = angle + 180;
        } else {
            angle += 360;
        }
    } else {
        angle = to_deg(atan(-x / y));
        
        if (y > 0 && -x > 0) {
            angle += 90;
        } else if (y < 0) {
            angle += 270;
        } else {
            angle += 90;
        }
    }
    
    return angle;
}

double to_deg(double rad) {
    return rad * (180 / (4 * atan(1)));
}

double to_rad(double deg) {
    return deg * ((4 * atan(1)) / 180);
}
