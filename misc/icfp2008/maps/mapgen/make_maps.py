#!/usr/bin/python
import sys
import random
from math import sqrt
from mapgen import *

def iscollision(origins, pt, r):
    for o in origins:
        if sqrt((o[0] - pt[0])**2 + (o[1] - pt[1])**2) < r+5:
            return True
    return False

def threerands(min1, max1, min2, max2):
    return (random.randint(min1, max1),
            random.randint(min1, max1),
            random.randint(min2, max2))

def manycraters():
    n = 200
    starts = [(0,50), (-25, 25), (0,0), (90, 90), (-90, 90)]
    cs = []
    for i in xrange(n):
        x, y, r = threerands(-100, 100, 1, 8)
        while iscollision(starts, (x,y), r):
            x, y, r = threerands(-100, 100, 1, 8)
        cs.append(Obstacle(x,y,r))

    runs = []
    for s in starts:
        runs.append(Run(Position(s[0], s[1], random.randint(-180, 180)), Enemies()))
        
    print Map(Spec(200, 30000), 
        Rover(), Martian(), 
        Craters(*cs), Boulders(), 
        Runs(*runs))

def manyboulders():
    n = 200
    starts = [(0,50), (-25, 25), (0,0), (90, 90), (-90, 90)]
    bs = []
    for i in xrange(n):
        x, y, r = threerands(-100, 100, 1, 8)
        while iscollision(starts, (x,y), r):
            x, y, r = threerands(-100, 100, 1, 8)
        bs.append(Obstacle(x,y,r))

    runs = []
    for s in starts:
        runs.append(Run(Position(s[0], s[1], random.randint(-180, 180)), Enemies()))
        
    print Map(Spec(200, 30000), 
        Rover(), Martian(), 
        Craters(), Boulders(*bs), 
        Runs(*runs))

def simple():
    print Map(Spec(200, 10000),
        Rover(), Martian(),
        Craters(),
        Boulders(Obstacle(0,30,2)),
        Runs(Run(Position(0,50,270), Enemies(Position(-5, 5, 135, .25, 60))),
             Run(Position(-25,25,-45), Enemies(Position(-5, 5, 135, .25, 60)))))

def lineofboulders():
    boulders = [Obstacle(x, 30, 2) for x in range(-10, 10)]
    print Map(
        Spec(200, 10000),
        Rover(), Martian(),
        Craters(),
        Boulders(*boulders),
        Runs(Run(Position(0,50,270), Enemies(Position(-5, 5, 135, .25, 60)))))

def trapped():
    boulders = [Obstacle(x, 30, 2) for x in range(-10, 10)]
    boulders.extend(Obstacle(-10, y, 2) for y in range(30, 50))
    boulders.extend(Obstacle(10, y, 2) for y in range(30, 50))
    print Map(
        Spec(200, 10000),
        Rover(), Martian(),
        Craters(),
        Boulders(*boulders),
        Runs(Run(Position(0,50,270), Enemies(Position(-5, 5, 135, .25, 60)))))

def trapped2():
    boulders = [Obstacle(x, 30, 2) for x in range(-10, 10, 2)]
    boulders.extend(Obstacle(-10, y, 2) for y in range(30, 50, 2))
    boulders.extend(Obstacle(10, y, 2) for y in range(30, 50, 2))
    print Map(
        Spec(200, 30000),
        Rover(), Martian(),
        Craters(),
        Boulders(*boulders),
        Runs(Run(Position(0,50,270), Enemies(Position(-5, 5, 135, .25, 60)))))

def maze():
    craters = [Obstacle(x, 50, 1) for x in range(-100, 90)]
    craters.extend(Obstacle(x, 70, 1) for x in range(-100, 90))
    craters.extend(Obstacle(x, 60, 1) for x in range(-90, 100))
    print Map(
        Spec(200, 30000),
        Rover(), Martian(),
        Craters(*craters),
        Boulders(),
        Runs(Run(Position(0,90,270), Enemies(Position(50, 80, 135, 20, 60)))))

def martians():
    badguys = [Position(x, 10, 100, 5, 120) for x in range(-50, 50, 5)]
    print Map(
        Spec(200, 30000),
        Rover(), Martian(),
        Craters(),
        Boulders(),
        Runs(Run(Position(0,90,270), Enemies(*badguys)),
             Run(Position(90,90,-80), Enemies(*badguys)),
             Run(Position(-90,90,0), Enemies(*badguys)),
             Run(Position(20,20,180), Enemies(*badguys)),
             Run(Position(90,10,10), Enemies(*badguys))))

def badlanding():
	s = 0.5 #1.5 still leaves enough space for the rover, I think
	craters = [Obstacle(5,5,2),Obstacle(-5,-5,2),Obstacle(5,-5,2),Obstacle(-5,5,2),
				Obstacle(-2,6.5,s),Obstacle(2,6.5,s),Obstacle(6.5,-2,s),Obstacle(6.5,2,s),
				Obstacle(2,-6.5,s),Obstacle(-2,-6.5,s),Obstacle(-6.5,2,s),Obstacle(-6.5,-2,s)
				]
	print Map(Spec(100,30000),
	Rover(),Martian(),Craters(*craters),Boulders(),
    Runs(Run(Position(0,90,270), Enemies()),
         Run(Position(90,90,-80), Enemies()),
         Run(Position(-90,90,0), Enemies()),
         Run(Position(20,20,180), Enemies()),
         Run(Position(90,10,10), Enemies())))
	
def fastblind():
    n = 100
    starts = [(0,90), (90, 90), (0,0), (90, 10), (20, 20), (-90, 90)]
    bs = []
    for i in xrange(n):
        x, y, r = threerands(-100, 100, 1, 2)
        while iscollision(starts, (x,y), r):
            x, y, r = threerands(-100, 100, 1, 2)
        bs.append(Obstacle(x,y,r))

    print Map(
        Spec(200, 30000),
        Rover(maxSpeed=100, accel=5, brake=5, turn=30, hardTurn=70, frontView=10, rearView=5),
        Martian(),
        Craters(),
        Boulders(*bs),
        Runs(Run(Position(0,90,270), Enemies()),
             Run(Position(90,90,-80), Enemies()),
             Run(Position(-90,90,0), Enemies()),
             Run(Position(20,20,180), Enemies()),
             Run(Position(90,10,10), Enemies())))

def ahh():
    n = 10000
    bigsize = 120
    bs = []
    starts = [(0,4000), (1000,0), (0,0), (-3000, -1000), (2000, -2000), (-900, 2400)]
    for i in xrange(n):
        x, y, r = threerands(-5000, 5000, 1, bigsize)
        while iscollision(starts, (x,y), (r/2)):
            x, y, r = threerands(-5000, 5000, 1, bigsize)
        bs.append(Obstacle(x,y,(r/2)))

    print Map(
        Spec(5000, 60000),
        Rover(maxSpeed=200, accel=10, brake=5, turn=30, hardTurn=70, frontView=150, rearView=15),
        Martian(),
        Craters(),
        Boulders(*bs),
        Runs(Run(Position(0,4000,270), Enemies()),
             Run(Position(1000,0,-80), Enemies()),
             Run(Position(-3000,-1000,0), Enemies()),
             Run(Position(2000,-2000,180), Enemies()),
             Run(Position(-900,2400,10), Enemies())))
	

if __name__=="__main__":
    globals()[sys.argv[1]]()
