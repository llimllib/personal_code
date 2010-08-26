
#include <stdlib.h>
#include <string.h>

#include "stored_objects.h"
#include "maths.h"


void free_object_list_alone(object_list *o) {
    object_list *tmp;
    
    if (!o) {
        return;
    }
    
    while (o->next) {
        tmp = o->next;
        free(o);
        o = tmp;
    }
    
    free(o);
}


void free_object_list_objects(object_list *o) {
    object_list *tmp;
    
    if (!o) {
        return;
    }
    
    while (o->next) {
        tmp = o->next;
        free(o->data);
        free(o);
        o = tmp;
    }
    
    free(o->data);
    free(o);
}

void push_object(object_list **list, object *o) {
    object_list *temp;
    
    temp = malloc(sizeof(object_list));
    temp->data = o;
    
    temp->next = (*list);
    *list = temp;
}

int list_contains_object(object_list *list, object *o) {
    while (list) {
        if (memcmp(o, list->data, sizeof(object)) == 0) {
            return 1;
        }
        list = list->next;
    }
    return 0;
}


void get_near_objects(object_list **out, object_list *in, double x, double y, double distance) {
    object *o;
    
    
    for (; in; in = in->next) {
        o = in->data;
        if (get_distance(x, y, o->x, o->y) < distance + o->a) {
            push_object(out, o);
        }
    }
    
}



