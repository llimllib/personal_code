import random


def factoradic(anInt,order=0):
   """calculate the factoradic on anInt

   >>> factoradic(859)
   [1, 1, 0, 3, 0, 1, 0]

   >>> factoradic(11233111122213455539988899978655326328)
   [1, 9, 22, 2, 20, 20, 7, 14, 0, 19, 2, 13, 2, 5, 14, 18, 2, 0, 10, 1, 9, 3, 11, 9, 9, 4, 1, 4, 0, 0, 1, 1, 0, 0]

   >>> factoradic(0,4)
   [0, 0, 0, 0]

   >>> factoradic(1)
   [1, 0]

   >>> factoradic(1047)
   [1, 2, 3, 2, 1, 1, 0]

   >>> factoradic(5,4)
   [0, 2, 1, 0]


   """

   factoradic = []

   z = 0
   while anInt > 0:
       z += 1
       factoradic.append(int(anInt % z))
       anInt /= z


   factoradic.reverse()
   if order:
       while len(factoradic) < order:
           factoradic.insert(0,0)

   return factoradic

def factorial(anInt):
   """factorial

   >>> factorial(3)
   6
   >>> factorial(0)
   1
   >>> factorial(1)
   1
   """
   if anInt == 0:
       return 1
   if anInt < 0:
       raise ValueError, "Cannot factorialize negative numbers"
   result = 1

   while anInt > 1:
       result = result * anInt
       anInt -= 1
   return result


def unfactoradic(aList):
   """from a factoradic list, calculate the integer

   >>> unfactoradic([1, 1, 0, 3, 0, 1, 0])
   859

   """
   aList.reverse()
   result = 0
   for idx,val in enumerate(aList):
       result += factorial(idx) * val
   return result



class Permutation(object):
   """Base object for doing permutations.  Generally initialized with a list
   of the items to do permutations on.  Works by the factoradic method,
   which provides reversibility."""

   _order = None

   def __init__(self,data):
       self.data = data

   def getOrder(self):
       if not self._order:
           self._order = len(self.data)
       return self._order

   def permutationIndices(self,anInt):
       """calculate the permutation indices of self from anInt

       >>> z = Permutation([1,2,3,4,5,6,7])
       >>> z.permutationIndices(1047)
       [1, 3, 5, 4, 2, 6, 0]
       >>> z = Permutation([0,1,2,3])
       >>> z.permutationIndices(5)
       [0, 3, 2, 1]


       """
       f = factoradic(anInt,self.order)
       temp = []
       for k in f:
           temp.append(k + 1)

       data = [1]
       temp.reverse()
       for k in temp[1:]:
           data.insert(0,k)
           for idx,val in enumerate(data[1:]):
               if val >= k:
                   data[idx+1] = val + 1
       for idx,val in enumerate(data):
           data[idx] = val-1
       return data


   def permutation(self,anInt):
       """return a list of permutated items

       >>> z = Permutation([1,2,3,4,5,6,7])
       >>> z.permutation(1047)
       [2, 4, 6, 5, 3, 7, 1]

       """
       indices = self.permutationIndices(anInt)
       newlist = []
       for k in indices:
           newlist.append(self.data[k])
       return newlist

   def randomPermutation(self):
       """just get one of them, randomly"""
       r = random.randint(0,factorial(self.order))
       return self.permutation(r)

   def getPermutationIndex(self,aPermutation):
       """presuming a unique list, get the permutation index of the given
       permutation list.

       >>> d = [1,2,3,4,5,6,7]
       >>> z = Permutation(d)
       >>> z.getPermutationIndex([2, 4, 6, 5, 3, 7, 1])
       1047
       """
       indexkey = []
       for k in aPermutation:
           indexkey.append(self.data.index(k))
       data = []
       for k in indexkey:
           data.append(k+1)
       factoradic = []
       while len(data) > 0:
           r = data.pop(0)
           factoradic.append(r-1)
           for idx,val in enumerate(data):
               if val >= r:
                   data[idx] = val -1
       return unfactoradic(factoradic)

   order = property(getOrder)

def listAll(anInt):
   theList = []
   for k in range(anInt):
       theList.append(k)
   z = Permutation(theList)
   for k in range(factorial(len(z.data))):
       b = factoradic(k,len(z.data))
       c = z.permutation(k)
       d = z.getPermutationIndex(c)
       print "%s\t%s\t%s\t%s" % (k,b,c,d)


def _test():
   import doctest,Permutation
   return doctest.testmod(Permutation)


if __name__ == '__main__':
   _test()
   listAll(4)
