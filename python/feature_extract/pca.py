from Numeric import *
from LinearAlgebra import *

class PCA:
    def __init__(self, matrix, M):
        """
        Assumes that each example in matrix is a column.
        """
        #TODO: sanity check input
        self.matrix = matrix
        self.d = len(self.matrix)     #dimensionality of raw array
        self.n = len(self.matrix[0])  #number of examples
        self.mean = self.getMean(self.matrix)
        self.covar = self.makeCovarMatrix(self.matrix, self.mean)
        self.pc = self.getPrimaryComponents(self.covar, M)
        self.pca = self.transform(self.matrix, self.pc)
    
    def transform(self, matrix, pc):
        return matrixmultiply(pc, matrix)

    def getMean(self, array):
        #do it easier with slices?
        sum = zeros((self.d, 1), Float)
        for i in range(self.d):
            for j in range(self.n):
                sum[i][0] += array[i][j]
        return sum / float(self.n)
    
    def makeCovarMatrix(self, array, mean):
        covar = zeros((self.d, self.d), Float)
        for i in range(self.n):
            diff = transpose([array[:,i]]) - mean
            diffMat = matrixmultiply(diff, transpose(diff))
            covar += diffMat
        return covar
    
    def getPrimaryComponents(self, covarMatrix, M):
        eval, evec = eigenvectors(covarMatrix)
        self.eval = eval.real
        self.evec = evec.real
        self.esort = sort(eval)
        pc = zeros((M, len(eval)), Float)
        for i in range(M):
            x = self.esort[-(i+1)]
            for j in range(len(self.eval)):
                if self.eval[j] == x:
                    pc[i] = self.evec[j]
        return pc

if __name__ == "__main__":
    fe = open('expert.dat')
    fr = open('random.dat')
    expert = []
    line = fe.readline()[:-2]
    while line:
        expert.append(line.split('\t'))
        line = fe.readline()[:-2]
    convert_to_float(expert)
    
    random = []
    line = fr.readline()[:-2]
    while line:
        random.append(line.split('\t'))
        line = fr.readline()[:-2]
    convert_to_float(random)

    pca = PCA(transpose(array(expert)), 12)
    return pca

def deleteme():
    

def convert_to_float(array):
    """Converts an array of vectors to integers"""
    for i in range(len(array)):
        for j in range(len(array[i])):
            array[i][j] = float(array[i][j])
