
#ifndef STORED_OBJECTS_H
#define STORED_OBJECTS_H

// will hold all types of map objects; a is for the diameter of boulders, craters, and home
// a and b is for direction and speed of martians
typedef struct object {
    char kind;
    double x;
    double y;
    double a;
    double b;
} object;


typedef struct object_list {
    struct object *data;
    struct object_list *next;
} object_list;



void free_object_list_alone(object_list *o);
void free_object_list_objects(object_list *o);
void push_object(object_list **list, object *o);
int list_contains_object(object_list *list, object *o);
void get_near_objects(object_list **out, object_list *in, double x, double y, double distance);



#endif // STORED_OBJECTS_H
