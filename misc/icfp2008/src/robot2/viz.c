
#include <stdio.h>
#include <SDL/SDL.h>
#include <math.h>


#include "viz.h"
#include "main.h"
#include "maths.h"
#include "stored_objects.h"

#if VIZ == 1

SDL_Surface *screen;
Uint32 current_color;


extern initialization last_init;

extern telemetry last_telemetry;

extern event last_event;


extern object_list *static_objects;

extern object_list *martians;



extern object_list *viz_checked_points;
extern int viz_decided_path;


void init_viz(void) {
    
    SDL_Init(SDL_INIT_VIDEO);
    
    screen = SDL_SetVideoMode(VIZ_WIDTH, VIZ_HEIGHT, 0, SDL_SWSURFACE);
    
}


void draw_point(int x, int y, int radius) {
    SDL_Rect rect;
    
    int iterations;
    int i;
    
    
    //if (radius < 2) {
    //    radius = 2;
    //}
    
    iterations = radius/2 + 1;
    
    for (i = 0; i < iterations; i++) {
        double angle;
        int ix;
        int iy;
        
        angle = (90.0 * (i + .5)) / (iterations);
        ix = radius * cos(to_rad(angle));
        iy = radius * sin(to_rad(angle));
        
        
        rect.x = x - ix;
        rect.y = y - iy;
        rect.w = 2*ix + 1;
        rect.h = 2*iy + 1;
        SDL_FillRect(screen, &rect, current_color);
        
    }
}

void plot_point(double x, double y, double radius) {
    int temp_x, temp_y;
    int temp_radius;
    double x1, y1;
    double diff_x, diff_y;

    x1 = -last_init.dx / 2;
    y1 = -last_init.dy / 2;
    
    diff_x = last_init.dx;
    diff_y = last_init.dy;
    
    temp_x = ((x - x1) / diff_x) * VIZ_WIDTH;
    temp_radius = (radius / diff_x) * VIZ_WIDTH;
    temp_y = ((-y - y1) / diff_y) * VIZ_HEIGHT;
    
    draw_point(temp_x, temp_y, temp_radius);
}
    
    

void draw_line(int x1, int y1, int x2, int y2) {
    int iterations = get_distance(x1, y1, x2, y2) * 1.5;
    int i;
    int dist_x = x2 - x1;
    int dist_y = y2 - y1;
    SDL_Rect rect;
    rect.w = 1;
    rect.h = 1;
    
    //SDL_LockSurface(screen);
    
    for (i = 0; i < iterations; i++) {
        double percent = i / (double) iterations;
        int temp_x = x1 + (percent * dist_x);
        int temp_y = y1 + (percent * dist_y);
        
        rect.x = temp_x;
        rect.y = temp_y;
        SDL_FillRect(screen, &rect, current_color);
    }
        
    //SDL_UnlockSurface(screen);
}


void plot_line(double x1, double y1, double x2, double y2) {
    int temp_x1, temp_y1, temp_x2, temp_y2;
    int temp_radius;
    double ix1, iy1;
    double diff_x, diff_y;
    

    ix1 = -last_init.dx / 2;
    iy1 = -last_init.dy / 2;
    
    diff_x = last_init.dx;
    diff_y = last_init.dy;
    
    temp_x1 = ((x1 - ix1) / diff_x) * VIZ_WIDTH;
    temp_x2 = ((x2 - ix1) / diff_x) * VIZ_WIDTH;
    temp_y1 = ((-y1 - iy1) / diff_y) * VIZ_HEIGHT;
    temp_y2 = ((-y2 - iy1) / diff_y) * VIZ_HEIGHT;
    
    draw_line(temp_x1, temp_y1, temp_x2, temp_y2);
}


void plot_vector(double x, double y, double angle, double magnitude) {
    double x2, y2;
    
    x2 = x + (cos(to_rad(angle)) * magnitude);
    y2 = y + (sin(to_rad(angle)) * magnitude);
    
    plot_line(x, y, x2, y2);
}
    


void set_color(double r, double g, double b) {
    current_color = SDL_MapRGB(screen->format, r*255, g*255, b*255);
}


void viz_update(void) {
    SDL_Rect rect;
    object_list *l;
    object *o;
    int i;
    object *last_object;
    object *last_point;
    
    
    // clear screen
    set_color(1, 1, 1);
    SDL_FillRect(screen, NULL, current_color);
    
    
    
    
    // draw known objects
    for (l = static_objects; l; l = l->next) {
        o = l->data;
        switch (o->kind) {
            case 'b':
                set_color(.4, .4, .4);
                break;
            case 'c':
                set_color(.7, .4, .3);
                break;
            case 'h':
                set_color(0, 1, 0);
                break;
            default:
                set_color(1, 0, 1);
                break;
        }
        plot_point(o->x, o->y, o->a);

    }
    for (l = martians; l; l = l->next) {
        set_color(1, 0, 0);
        o = l->data;
        plot_point(o->x, o->y, 3);
        plot_vector(o->x, o->y, o->a, o->b);
    }
    
    last_point = NULL;
    last_object = NULL;
    for (l = viz_checked_points; l; l = l->next) {
        o = l->data;
        
        if (last_object == NULL) {
            last_object = o;
        }
        
        if (last_point == NULL) {
            last_point = o;
            i = 0;
        }
        
        if (last_object->kind != viz_decided_path) {
            set_color(.5, .5, 1);
        } else {
            set_color(0, 0, 1);
        }
        
        if (last_object->kind != o->kind) {
            plot_line(last_point->x, last_point->y, last_object->x, last_object->y);
            last_point = NULL;
            i = 0;
        }
        
        if (i > 5) {
            plot_line(last_point->x, last_point->y, o->x, o->y);
            last_point = o;
            i = 0;
        }
        
        last_object = o;
        
        i++;
    }
    if (last_point && last_object) {
        plot_line(last_point->x, last_point->y, last_object->x, last_object->y);
    }
    
    // draw destination
    //set_color(.5, .5, 1);
    //plot_point(destination_x, destination_y, 0);
    
    // draw rover
    set_color(0, .5, 0);
    plot_point(last_telemetry.vehicle_x, last_telemetry.vehicle_y, 3);
    
    // draw rover's vector
    plot_vector(last_telemetry.vehicle_x, last_telemetry.vehicle_y, last_telemetry.vehicle_dir, last_telemetry.vehicle_speed);
    
    SDL_Flip(screen);
    
    //fprintf(stderr, "Done\n");
}

#endif
