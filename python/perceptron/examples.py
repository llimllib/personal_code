#! /usr/bin/env python
#pythonpath imports
import random
import numarray as na
#local imports
from plots import *
from perceptron import *

def solve_logical_examples():
    #create arrays (0,0), (0,1), (1,0), (1,1)
    net_input = [na.array((x1,x2)) for x1 in range(2) for x2 in range(2)]
    # create expected output
    net_output = [-1,1,1,1]
    
    a,b = dual_perceptron(net_input, net_output)
    print "dual form: ", a, b
    
    c,d,k = perceptron(net_input, net_output, .5)
    print "perceptron: ", c, d

    print "printing perceptron output: "
    m = -c[0] / c[1]
    d = -d / c[1]
    plot_line(m, d, (-.5, 1.5), (-.5, 1.5))
    
    #solve for the weight vector
    sum_j = na.zeros(2, na.Float32)
    for j in range(len(net_input)):
        sum_j += a[j] * net_output[j] * net_input[j]
    print "sum_j: ", sum_j
    
    #solve for x2 (which is sum_j[1]); assume 2-d
    b = -b / sum_j[1]
    co_x1 = -sum_j[0] / sum_j[1]

    print "printing dual form perceptron output: " 
    plot_line(co_x1, b, (-.5, 1.5), (-.5, 1.5))

def solve_apples_bananas():
    print "generating points"
    pts = gen_random_points(100)
    bananas = [pt[0] for pt in pts if pt[1] < 0]
    apples = [pt[0] for pt in pts if pt[1] > 0]
    plot_bananas(bananas, apples, 'points.png')
    
    print "running perceptron"
    cat = na.array([pt[1] for pt in pts])
    pts = na.array([pt[0] for pt in pts])
    w, b, k = perceptron(pts, cat, .3, 0)

    print "perceptron output: ", w, b, k
    m = -w[0] / w[1]
    b = -b / w[1]
    plot_bananas(bananas, apples, 'line.png', m, b)

def gen_random_points(n):
    """generate n random linearly seperable points"""
    r = random.Random()
    points = []
    count = 0
    while count < n:
        x, y = (r.randint(0,99), r.randint(0,99))
        if 100 - x > y + 1:
            points.append(((x,y),-1))
            count += 1
        elif 100 - x < y - 1:
            points.append(((x,y),1))
            count += 1
    return points

if __name__ == "__main__":
    #solve_logical_examples()
    solve_apples_bananas()
