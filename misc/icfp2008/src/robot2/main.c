
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>


#include "main.h"
#include "stored_objects.h"
#include "process_data.h"
#include "viz.h"



initialization last_init;

telemetry last_telemetry;
telemetry last_telemetry_p;

event last_event;


object_list *static_objects;

object_list *martians;

//object_list *visited_locations;

extern long int last_calculation;

double accel;

    

int main (int argc, char **argv) {
    // temporary character we read in
    int temp;
    
    // temp object
    object *o;
    
    // these are solely for convenience sake
    initialization *i = &last_init;
    telemetry *t = &last_telemetry;
    event *e = &last_event;
    
    #if VIZ == 1
        init_viz();
    #endif
    
    while (1) {
        do {
            temp = getchar();
        } while (temp == ' ' || temp == '\n');
        
        if (temp == -1) {
            break;
        }
        
        switch (temp) {
            // initialization
            case 'I':
                //free_object_list_objects(visited_locations);
                //visited_locations = NULL;
                scanf("%lf %lf %ld %lf %lf %lf %lf %lf ;",
                        &i->dx, &i->dy, &i->time_limit, &i->min_sensor,
                        &i->max_sensor, &i->max_speed, &i->max_turn, &i->max_hard_turn);
                
                
                // c'mon, give me something to work with!
                accel = last_init.max_speed;
                break;
            // telemetry
            case 'T':
                
                memcpy(&last_telemetry_p, &last_telemetry, sizeof(telemetry));
                
                free_object_list_objects(martians);
                martians = NULL;
                
                free_object_list_objects(static_objects);
                static_objects = NULL;
                
                scanf("%ld %2c %lf %lf %lf %lf", &t->time_stamp,
                        &t->vehicle_control[0], &t->vehicle_x, &t->vehicle_y, 
                        &t->vehicle_dir, &t->vehicle_speed);
                
                if (last_telemetry.vehicle_speed < .5 * last_init.max_speed
                        && last_telemetry.vehicle_control[0] == 'a') {
                    accel = (last_telemetry.vehicle_speed - last_telemetry_p.vehicle_speed) / (last_telemetry.time_stamp - last_telemetry_p.time_stamp);
                }
                
                while (1) {
                    do {
                        temp = getchar();
                    } while (temp == ' ' || temp == '\n');
                    
                    if (temp == -1 || temp == ';') {
                        break;
                    }
                    
                    // boulder, crater, or home (doesn't matter much to us)
                    if (temp == 'b' || temp == 'c' || temp == 'h') {
                        o = malloc(sizeof(object));
                        o->kind = temp;
                        scanf("%lf %lf %lf", &o->x, &o->y, &o->a);
                        
                        //if (!list_contains_object(static_objects, o)) {
                            push_object(&static_objects, o);
                        //    //fprintf(stderr, "Added static %c\n", temp);
                        //} else {
                        //    free(o);
                        //}
                    }
                    
                    // martian
                    if (temp == 'm') {
                        o = malloc(sizeof(object));
                        o->kind = temp;
                        scanf("%lf %lf %lf %lf", &o->x, &o->y, &o->a, &o->b);
                        
                        push_object(&martians, o);
                    }
                }
                
                if (last_telemetry.time_stamp < last_calculation || last_telemetry.time_stamp - UPDATE >= last_calculation) {
                    recalculate_destination();
                }
                
                aim_for_destination();
                
                #if VIZ == 1
                viz_update();
                #endif
                
                break;
            // bumped into boulder
            case 'B':
            // fell into krater and died
            case 'C':
            // a martian got us
            case 'K':
            // we WON!?!?
            case 'S':
                // todo: do something, perhaps?
                
                e->type = temp;
                scanf("%ld ;", &e->time_stamp);
                break;
            // game over!
            case 'E':
                e->type = temp;
                scanf("%ld %ld ;", &e->time_stamp, &e->score);
                //free_object_list_objects(visited_locations);
                //visited_locations = NULL;
                last_calculation = 0;
                break;
        }
    }
}

