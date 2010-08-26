

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "main.h"
#include "stored_objects.h"
#include "process_data.h"
#include "maths.h"


extern initialization last_init;

extern telemetry last_telemetry;
extern telemetry last_telemetry_p;

extern event last_event;


extern object_list *static_objects;

extern object_list *martians;

extern double accel;

//extern object_list *visited_locations;

#if VIZ == 1

object_list *viz_checked_points;
int viz_decided_path;

#endif

char turn_codes[] = "Ll-rR";

double turn_thresholds[5];


long int last_calculation = 0;
//double destination_x, destination_y;

double target_angle;
double target_speed;



// todo: make more worthwhile
void aim_for_destination(void) {
    char buf[10];
    int cnt = 0;
    int i;
    int need_to_turn;
    int state_turn_code;
    
    // convenience
    char state_accel = last_telemetry.vehicle_control[0];
    char state_turn = last_telemetry.vehicle_control[1];
    double vehicle_x = last_telemetry.vehicle_x;
    double vehicle_y = last_telemetry.vehicle_y;
    double vehicle_dir = last_telemetry.vehicle_dir;
    
    double actual_angle;
    int found_angle;
    
    
    
    //if (get_distance(vehicle_x, vehicle_y, 0, 0) > last_init.max_speed) {
    //    target_speed = last_init.max_speed * 2;
    //} else {
    //    target_speed = get_distance(vehicle_x, vehicle_y, 0, 0)*.3 + last_init.max_speed*.1;
    //}
    
    if (fabs(last_telemetry.vehicle_speed - target_speed) < .2 * target_speed) {
        if (state_accel == 'b') {
            buf[cnt++] = 'a';
        }
        if (state_accel == 'a') {
            buf[cnt++] = 'b';
            //fprintf(stderr, "Braking!\n");
        }
    } else if (last_telemetry.vehicle_speed > target_speed) {
        buf[cnt++] = 'b';
        //fprintf(stderr, "Braking!\n");
    } else {
        buf[cnt++] = 'a';
    }
    
    for (i = 0; i < 5; i++) {
        if (state_turn == turn_codes[i]) {
            state_turn_code = i;
        }
    }
    
    
    turn_thresholds[0] = last_init.max_hard_turn;
    turn_thresholds[1] = last_init.max_turn;
    turn_thresholds[2] = 0;
    turn_thresholds[3] = -last_init.max_turn;
    turn_thresholds[4] = -last_init.max_hard_turn;
    
    actual_angle = (vehicle_dir - vehicle_dir) / (last_telemetry.time_stamp - last_telemetry_p.time_stamp);
    
    // turning left more than we should
    if (actual_angle > target_angle) {
        // fallback
        need_to_turn = 4;
        found_angle = 0;
        for (i = 0; i < 5 && !found_angle; i++) {
            if (turn_thresholds[i] <= target_angle) {
                need_to_turn = i;
                found_angle = 1;
            }
        }
    } else {
        // turning right more than we should
        // fallback
        need_to_turn = 0;
        found_angle = 0;
        for (i = 4; i >= 0 && !found_angle; i--) {
            if (turn_thresholds[i] >= target_angle) {
                need_to_turn = i;
                found_angle = 1;
            }
        }
    }
    
    fprintf(stderr, "%lf\n", target_angle);
    
    /*origin_angle = get_angle(vehicle_x, vehicle_y, destination_x, destination_y);
    
    temp_angle = 720 + vehicle_dir - origin_angle;
    while (temp_angle >= 360) {
        temp_angle -= 360;
    }
    

    
    //fprintf (stderr, "vehicle_dir: % 7.2lf;    origin_angle: % 7.2lf;    temp_angle: % 7.2lf\n", vehicle_dir, origin_angle, temp_angle);
    //fprintf (stderr, "    vehicle_x: %lf; vehicle_y: %lf\n", vehicle_x, vehicle_y);
    
    if (temp_angle < SOFT_TURN_THRESHOLD) {
        need_to_turn = 2;
    } else if (temp_angle < HARD_TURN_THRESHOLD) {
        need_to_turn = 3;
    } else if (temp_angle < 180) {
        need_to_turn = 4;
    } else if (temp_angle < 360 - HARD_TURN_THRESHOLD) {
        need_to_turn = 0;
    } else if (temp_angle < 360 - SOFT_TURN_THRESHOLD) {
        need_to_turn = 1;
    } else {
        need_to_turn = 2;
    }*/
    
    
    if (need_to_turn < state_turn_code) {
        buf[cnt++] = 'l';
    }
    
    if (need_to_turn > state_turn_code) {
        buf[cnt++] = 'r';
    }
    
    buf[cnt++] = 0;
    
    printf("%s;\n", &buf[0]);
    fflush(NULL);

}






void recalculate_destination(void) {
    int i;
    //int j;
    double k;
    
    object *o;
    object_list *l;
    object_list *to_check = NULL;
    
    double distance;
    
    double temp_target_speed = last_init.max_speed * 2;
    
    double temp_values[NUMBER_DIRECTION_CHECKS];
    
    double temp_max;
    int temp_decided_path;
    
    
    #if VIZ == 1
    
    free_object_list_objects(viz_checked_points);
    viz_checked_points = NULL;
    
    #endif
    
    //fprintf(stderr, "%ld\n", last_telemetry.time_stamp);
    
    
    //get_near_objects(&to_check, static_objects, last_telemetry.vehicle_x, last_telemetry.vehicle_y, (last_init.max_sensor) + .5);
    
    for (l = static_objects; l; l = l->next) {
        push_object(&to_check, l->data);
    }
    for (l = martians; l; l = l->next) {
        push_object(&to_check, l->data);
    }
    //for (l = visited_locations; l; l = l->next) {
    //    push_object(&to_check, l->data);
    //}
    
    
    
    for (i = 0; i < NUMBER_DIRECTION_CHECKS; i++) {
        double angle, angle_per_tick;
        double temp_x, temp_y;
        double temp_speed, temp_dir;
        double accel_per_tick;
        
        double temp_value;
        
        double temp_time_increment;
        double temp_rover_size;
        
        
        
        
        angle = ((last_init.max_hard_turn * 2) * ((double) i / (double)(NUMBER_DIRECTION_CHECKS-1)))  - last_init.max_hard_turn;
        
        
        
        temp_x = last_telemetry.vehicle_x;
        temp_y = last_telemetry.vehicle_y;
        temp_speed = last_telemetry.vehicle_speed;
        temp_dir = last_telemetry.vehicle_dir;
        
        //for (j = 0; j < (last_init.max_sensor / INCREMENT); j++) {
        for (k = 0; k < last_init.max_sensor; k += temp_time_increment * last_init.max_speed) {
            object *temp_viz;
            
            int collision = 0;
            
            double martian_x, martian_y, martian_radius;
            
            
            if (k < last_init.max_sensor * .2) {
                temp_time_increment = TIME_INCREMENT;
                temp_rover_size = ROVER_BUFFER;
            } else {
                temp_time_increment = TIME_INCREMENT * (1 + (k/(TIME_INCREMENT*last_init.max_speed)) * 0.05);
                temp_rover_size = ROVER_BUFFER * (1 + (k/(TIME_INCREMENT*last_init.max_speed)) * 0.05);
            }
            
            angle_per_tick = angle * temp_time_increment;
            accel_per_tick = accel * temp_time_increment;
            
            
            if (temp_speed < temp_target_speed) {
                temp_speed += accel_per_tick;
                
                if (temp_speed > temp_target_speed) {
                    temp_speed = temp_target_speed;
                }
                if (temp_speed > last_init.max_speed) {
                    temp_speed = last_init.max_speed;
                }
            }
            
            // todo: replicate for temp_speed > temp_target_speed
            
            temp_x += cos(to_rad(temp_dir)) * temp_speed * temp_time_increment;
            temp_y += sin(to_rad(temp_dir)) * temp_speed * temp_time_increment;
            
            temp_dir += angle_per_tick;
            
            //fprintf(stderr, "%lf\n", k/(TIME_INCREMENT*last_init.max_speed));
            
            
            #if VIZ == 1
            temp_viz = malloc(sizeof(object));
            temp_viz->kind = i;
            temp_viz->x = temp_x;
            temp_viz->y = temp_y;
            
            push_object(&viz_checked_points, temp_viz);
            
            #endif
            
            temp_value = logf(k/(TIME_INCREMENT*last_init.max_speed)) + 2.5*(get_distance(last_telemetry.vehicle_x, last_telemetry.vehicle_y, 0, 0) - get_distance(temp_x, temp_y, 0, 0)) / (last_init.max_sensor);
            
            for (l = to_check; l; l = l->next) {
                o = l->data;
                distance = get_distance(temp_x, temp_y, o->x, o->y);
                
                switch (o->kind) {
                    case 'b':
                    case 'c':
                        if (distance < temp_rover_size + o->a) {
                            collision = 1;
                        }
                        break;
                    case 'm':
                        martian_x = o->x + cos(to_rad(o->a)) * o->b * k/(last_init.max_speed);
                        martian_y = o->y + sin(to_rad(o->a)) * o->b * k/(last_init.max_speed);
                        martian_radius = .4 + (k/(TIME_INCREMENT*last_init.max_speed)) * MARTIAN_SIZE_INCREASE_PER_TICK;
                        distance = get_distance(temp_x, temp_y, martian_x, martian_y);
                        if (distance < temp_rover_size + martian_radius) {
                            collision = 1;
                        }
                        break;
                    case 'h':
                        //if (temp_time_increment == TIME_INCREMENT) {
                        if (k < last_init.max_sensor * .4) {
                            if (distance < o->a / 4) {
                                // arbitrarily high
                                temp_value = 100000;
                                collision = 1;
                            }
                        } else {
                            if (distance < (temp_rover_size/4) * o->a) {
                                // arbitrarily high
                                temp_value = 100000;
                                collision = 1;
                            }
                        }
                        break;
                }
            }
            
            if (collision) {
                break;
            }
        }
        
        
        temp_values[i] = temp_value;
    }
    
    temp_max = 0;
    for (i = 1; i < NUMBER_DIRECTION_CHECKS-1; i++) {
        double temp_temp;
        
        temp_temp = (temp_values[i-1] < temp_values[i]) ? temp_values[i-1] : temp_values[i];
        temp_temp = (temp_temp < temp_values[i+1]) ? temp_temp : temp_values[i+1];
        
        if (temp_temp > temp_max) {
            temp_max = temp_temp;
            temp_decided_path = i;
        }
    }
    
    #if VIZ == 1
    viz_decided_path = temp_decided_path;
    #endif
    
    target_angle = ((last_init.max_hard_turn * 2) * ((double) temp_decided_path / (double)(NUMBER_DIRECTION_CHECKS-1)))  - last_init.max_hard_turn;
    
    
    target_speed = temp_target_speed;
    
    free_object_list_alone(to_check);
    
    last_calculation = last_telemetry.time_stamp;
    
    
    
    /*for (i = 0; i < NUMBER_DIRECTION_CHECKS; i++) {
        double temp_max_so_far = 0;
        double j;
        double turn_scale;
        double direction;
        double temp;
        double temp_max;
        double temp_angle;
        double x;
        double y;
        int can_reach_home = 0;
        double offset_direction;
        int far;
        
        direction = (i * 360.0) / NUMBER_DIRECTION_CHECKS;
        
        can_reach_home = 0;
        
        temp_angle = 720 + direction - last_telemetry.vehicle_dir;
        while (temp_angle >= 180) {
            temp_angle -= 360;
        }
        
        if (temp_angle > 0) {
            offset_direction = direction + 90;
        } else {
            offset_direction = direction - 90;
        }
        
        turn_scale = ((fabs(temp_angle))/180) * TURN_SCALE;
        //turn_scale = 0;
        
        //fprintf(stderr, "%lf, %lf\n", turn_scale, direction);
        
        for (j = 0; j < (TICK * (last_init.max_speed + ROVER_SIZE/2) ) / 1000; j+= INCREMENT) {
            double temp_size;

            double percent_j = (j / (last_init.max_speed * (TICK / 1000)));
            double distance;
            
            //fprintf(stderr, "%lf\n", percent_j);
            
            temp_size = ROVER_BUFFER + ROVER_BUFFER * turn_scale * (1 - percent_j) ;
            //temp_size = ROVER_BUFFER;
            
            
            
            x = cos(to_rad(direction)) * percent_j * last_init.max_speed * (TICK / 1000) + last_telemetry.vehicle_x; //+ cos(to_rad(offset_direction)) * ROVER_BUFFER * turn_scale * (1 - percent_j) * .5;
            y = sin(to_rad(direction)) * percent_j * last_init.max_speed * (TICK / 1000) + last_telemetry.vehicle_y; //+ sin(to_rad(offset_direction)) * ROVER_BUFFER * turn_scale * (1 - percent_j) * .5;
            
            for (l = to_check; l; l = l->next) {
                o = l->data;
                distance = get_distance(x, y, o->x, o->y);
                temp_max = 0;
                
                if (percent_j > .5) {
                    far = 1;
                } else {
                    far = 0;
                }
                
                switch (o->kind) {
                    case 'c':
                        if (distance < o->a + temp_size) {
                            temp_max = 1000;
                            //fprintf(stderr, "Crater at %lf\n",  direction);
                        } else if (distance < o->a + temp_size + ROVER_SIZE) {
                            temp_max = 500 - far*300;
                        }
                        break;
                    case 'm':
                        temp = MARTIAN_SIZE + (percent_j * (TICK * o->b) / 1000);
                        if (distance < temp * .5 + temp_size) {
                            temp_max = 800 - far*300;
                        } else if (distance < temp * .8 + temp_size) {
                            temp_max = 300 - far*200;
                        } else if (distance < temp * 1.3 + temp_size) {
                            temp_max = 100 - far*100;
                        }
                        break;
                    case 'b':
                        if (distance < o->a + temp_size) {
                            temp_max = 150 - far*100;
                        }
                        break;
                    case 't':
                        if (distance < o->a) {
                            temp_max = 50 - far*50;
                        }
                        break;
                    case 'h':
                        if (distance < o->a + ROVER_SIZE/2) {
                            can_reach_home = 1;
                        }
                        break;
                }
                temp_max_so_far = (temp_max_so_far > temp_max) ? temp_max_so_far : temp_max;
            }
        }
        
        
        
        if (temp_max_so_far <= 100 && can_reach_home) {
            temp_max_so_far = -1000;
        }
        
        temp_max_so_far += 30 * (get_distance(x, y, 0, 0) - get_distance(last_telemetry.vehicle_x, last_telemetry.vehicle_y, 0, 0)) / (TICK * last_init.max_speed / 1000);
        

        
        //fprintf(stderr, "%lf, %lf\n", direction, temp_max_so_far);

        
        if (temp_max_so_far < min_so_far) {
            min_so_far = temp_max_so_far;
            temp_x = x;
            temp_y = y;
        }
    }
    destination_x = temp_x;
    destination_y = temp_y;
    free_object_list_alone(to_check);
    
    
    //fprintf(stderr, "%lf, %lf\n", destination_x, destination_y);
    
    //o = malloc(sizeof(object));
    
    //o->kind = 't';
    //o->x = last_telemetry.vehicle_x;
    //o->y = last_telemetry.vehicle_y;
    //o->a = last_init.max_speed * (UPDATE/1000);
    
    //push_object(&visited_locations, o);
    */
    
}
    
    

