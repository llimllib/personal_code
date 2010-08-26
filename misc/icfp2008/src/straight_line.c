
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// will hold all types of map objects; a is for the diameter of boulders, craters, and home
// a and b is for direction and speed of martians
typedef struct object {
    char kind;
    double x;
    double y;
    double a;
    double b;
} object;

// these are for when we get an initialization message
double dx, dy, min_sensor, max_sensor, max_speed, max_turn, max_hard_turn;
long int time_limit;

// these are for when we get a telemetry message
long int time_stamp;
char vehicle_control[2];
double vehicle_x, vehicle_y, vehicle_dir, vehicle_speed;

// these are for when we get any other message
long int throwaway_time_stamp, score;


// todo: make more worthwhile
void process_data(void) {
    char buf[10];
    int cnt = 0;
    
    double origin_angle, temp_angle;
    
    origin_angle = atan(vehicle_y / vehicle_x) * (180 / (4 * atan(1)));
    
    // todo: check for zeros!!
    if (vehicle_x > 0 && vehicle_y > 0) {
        // do nothing
    } else if (vehicle_x < 0) {
        origin_angle = origin_angle + 180;
    } else {
        origin_angle = origin_angle + 360;
    }
    
    
    if (vehicle_control[0] != 'a') {
        buf[cnt++] = 'a';
    }
    
    temp_angle = 720 + vehicle_dir - origin_angle;
    while (temp_angle >= 360) {
        temp_angle -= 360;
    }
    
    //fprintf (stderr, "vehicle_dir: % 7.2lf;    origin_angle: % 7.2lf;    temp_angle: % 7.2lf\n", vehicle_dir, origin_angle, temp_angle);
    //fprintf (stderr, "    vehicle_x: %lf; vehicle_y: %lf\n", vehicle_x, vehicle_y);
    
    if (temp_angle > 180 + 20 && temp_angle < 360) {
        if (vehicle_control[1] != 'r' && vehicle_control[1] != 'R') {
            buf[cnt++] = 'r';
        }
    } else if (temp_angle < 180 - 20 && temp_angle > 0) {
        
        if (vehicle_control[1] != 'l' && vehicle_control[1] != 'L') {
            buf[cnt++] = 'l';
        }
    } else {
        if (vehicle_control[1] == 'l' || vehicle_control[1] == 'L') {
            buf[cnt++] = 'r';
        } else if (vehicle_control[1] == 'r' || vehicle_control[1] == 'R') {
            buf[cnt++] = 'l';
        }
    }
    
    buf[cnt++] = 0;
    
    printf("%s;\n", &buf[0]);
    fflush(NULL);

}
    

int main (int argc, char **argv) {
    int temp;
    object *temp_object;
    
    while (1) {
        do {
            temp = getchar();
        } while (temp == ' ');
        
        if (temp == -1) {
            break;
        }
        
        switch (temp) {
            // initialization
            case 'I':
                scanf("%lf %lf %ld %lf %lf %lf %lf %lf ;",
                        &dx, &dy, &time_limit, &min_sensor,
                        &max_sensor, &max_speed, &max_turn, &max_hard_turn);
                break;
            // telemetry
            case 'T':
                scanf("%lf %2c %lf %lf %lf %lf", &time_stamp,
                        &vehicle_control[0], &vehicle_x, &vehicle_y, 
                        &vehicle_dir, &vehicle_speed);
                while (1) {
                    do {
                        temp = getchar();
                    } while (temp == ' ');
                    
                    if (temp == -1 || temp == ';') {
                        break;
                    }
                    
                    // boulder, crater, or home (doesn't matter much to us)
                    if (temp == 'b' || temp == 'c' || temp == 'h') {
                        temp_object = malloc(sizeof(object));
                        temp_object->kind = temp;
                        scanf("%lf %lf %lf", &temp_object->x,
                                &temp_object->y, &temp_object->a);
                        
                        // TODO: add to a tree
                        free(temp_object);
                    }
                    
                    // martian
                    if (temp == 'm') {
                        temp_object = malloc(sizeof(object));
                        temp_object->kind = temp;
                        scanf("%lf %lf %lf %lf", &temp_object->x,
                                &temp_object->y, &temp_object->a,
                                &temp_object->b);
                        
                        // TODO: add to a tree
                        free(temp_object);
                    }
                }
                
                // every time we get telemetry, we should do something with it
                process_data();
                break;
            // bumped into boulder
            case 'B':
            // fell into krater and died
            case 'C':
            // a martian got us
            case 'K':
            // we WON!?!?
            case 'S':
                // do something 
                scanf("%ld ;", &throwaway_time_stamp);
                break;
            // game over!
            case 'E':
                scanf("%ld ;", &throwaway_time_stamp, &score);
                break;
        }
    }
}

