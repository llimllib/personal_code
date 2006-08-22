#!/usr/bin/env python
import numarray as na
import math

def dual_perceptron(x, y):
    """implements a dual form perceptron
    
    as defined in "Support Vector Machines", Cristiani, p.18
    @x is a list of i 'numarray.array's that define the input
    @y is a list of i correct outputs for x, must be syncronized with x
        i.e. y[3] == correct f(x[3])
    @n is the learning rate 0 < n < 1"""
    a = na.zeros(len(x), na.Float32)     #embedding strength = alpha
    b = 0                                       #bias
    R = math.pow(max_norm(x), 2)
    mistake_free = 0
    iteration = 0
    while mistake_free == 0 and iteration < 50:
        iteration += 1
        print "iteration #", iteration
        mistake_free = 1
        for i in range(len(x)):
            sum_lc = 0
            for j in range(len(x)):
                sum_lc += a[j] * y[j] * na.dot(x[j], x[i]) #+ b
            print sum_lc, a, y, x, b
            if y[i] * (sum_lc + b) <= 0:
                print b, a, sum_lc, y[i]
                a[i] += 1
                b += y[i] * R
                mistake_free = 0
    return (a, b)

def perceptron(x, y, n, debug = 0):
    """implements a perceptron
    
    as defined in "Support Vector Machines", Cristiani, p.12
    @x is a list of i 'numarray.array's
    @y is a list of i correct outputs for x
    @n is the learning rate 0 < n < 1"""
    w = na.zeros(x[0].shape[0], na.Float32) #weights
    b = 0                                   #bias
    k = 0                                   #error count
    R = max_norm(x) ** 2
    if(debug): print "R= ", R
    iteration = 0                           #number of iterations
    mistake_free = 0
    while mistake_free == 0 and iteration < 100:
        iteration += 1
        if(debug): print "iteration #", iteration
        mistake_free = 1
        for i in range(len(x)):
            input_vec = x[i]
            expected_out = y[i]
            actual_out = y[i]*(na.dot(w, x[i]) + b)
            if actual_out <= 0:
                w += n * y[i] * x[i]
                if(debug): print n, y[i], x[i], w, b
                b += n + (y[i] - (na.dot(w, x[i]) + b)) # * R
                k += 1
                mistake_free = 0
    return (w, b, k)

def max_norm(array):
    """Returns the max norm of the network input elements in array"""
    norms = []
    for vector in array:
        sum_sq = 0
        for x in vector:
            sum_sq += x*x
        norms.append(math.sqrt(sum_sq))
    return max(norms)


