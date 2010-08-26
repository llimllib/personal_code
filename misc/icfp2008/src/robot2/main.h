
#ifndef MAIN_H
#define MAIN_H

#define VIZ 1


typedef struct initialization {
    double dx;
    double dy;
    long int time_limit;
    double min_sensor;
    double max_sensor;
    double max_speed;
    double max_turn;
    double max_hard_turn;
} initialization;


typedef struct telemetry {
    long int time_stamp;
    char vehicle_control[2];
    double vehicle_x;
    double vehicle_y;
    double vehicle_dir;
    double vehicle_speed;
} telemetry;


typedef struct event {
    char type;
    long int time_stamp;
    long int score;
} event;




#endif // MAIN_H
