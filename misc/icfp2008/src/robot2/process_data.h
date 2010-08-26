
#ifndef PROCESS_DATA_H
#define PROCESS_DATA_H

// milliseconds
//#define TICK (2000.0)
#define UPDATE (100.0)

//#define SOFT_TURN_THRESHOLD (7)
//#define HARD_TURN_THRESHOLD (15)

#define ROVER_SIZE (.5)
#define ROVER_BUFFER (ROVER_SIZE * 1.5)
#define INCREMENT (ROVER_BUFFER * .66)
#define TIME_INCREMENT (INCREMENT / last_init.max_speed)
#define TURN_SCALE (10.0)

#define MARTIAN_SIZE_INCREASE_PER_SECOND (5.0)
#define MARTIAN_SIZE_INCREASE_PER_TICK (MARTIAN_SIZE_INCREASE_PER_SECOND * TIME_INCREMENT)

#define NUMBER_DIRECTION_CHECKS (21)

#define MARTIAN_SIZE (.4)

void aim_for_destination(void);
void recalculate_destination(void);

#endif // PROCESS_DATA_H

