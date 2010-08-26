#!/bin/sh

gcc viz.c maths.c process_data.c stored_objects.c main.c -lm `sdl-config --libs` -o ../../bin/robot2 
chmod +x ../../bin/robot2
