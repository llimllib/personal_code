from pyevolve import G1DList
from pyevolve import GSimpleGA
from pyevolve import Selectors

def eval_func(ind):
   score = 0.0
   for x in range(0,len(ind)):
      if ind[x]==0: score += 0.1
   return score



