from Numeric import *
from PCA import PCA
import sys, os

def test():
    a = array([[5, 9, 7],[3, 7, 4],[2, 3, 9]])
    pca = PCA(a, 2)
    print "mean:  %s" % pca.mean
    print "covar: %s" % pca.covar
    print "eval:  %s" % pca.eval
    print "evec:  %s" % pca.evec
    print "esort: %s" % pca.esort
    print "pc:    %s" % pca.pc
    print "pca:   %s" % pca.pca

if __name__ == "__main__":
    test()
