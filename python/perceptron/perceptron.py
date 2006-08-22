import numarray as na
import math

def perceptron(x, y, n, debug = 0):
    """implements a perceptron
    
    modified from "Support Vector Machines", Cristiani, p.12
    @x is a list of i 'numarray.array's
    @y is a list of i correct outputs for x
    @n is the learning rate 0 < n < 1
    @debug is 1 for debugging, 0 for silent"""
    w = na.zeros(x[0].shape[0], na.Float32) #weights
    b = 0                                   #bias
    k = 0                                   #error count
    iteration = 0                           #number of iterations
    mistake_free = 0
    while not mistake_free and iteration < 100:
        iteration += 1
        if(debug): print "iteration #", iteration
        mistake_free = 1
        for i in range(len(x)):
            actual_out = na.dot(w, x[i]) + b #<w*x> + b
            if y[i] * actual_out <= 0:
                w += n * y[i] * x[i]
                if(debug): print n, y[i], x[i], w, b
                b += y[i] - actual_out
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


