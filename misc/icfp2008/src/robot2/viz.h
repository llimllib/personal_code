

#ifndef VIZ_H
#define VIZ_H

#include <SDL/SDL.h>

#include "main.h"

#if VIZ == 1

#define VIZ_WIDTH 500
#define VIZ_HEIGHT 500

void init_viz(void);
void draw_point(int x, int y, int radius);
void plot_point(double x, double y, double radius);
void draw_line(int x1, int y1, int x2, int y2);
void plot_line(double x1, double y1, double x2, double y2);
void plot_vector(double x, double y, double angle, double magnitude);
void set_color(double r, double g, double b);
void viz_update(void);

#endif

#endif // VIZ_H
