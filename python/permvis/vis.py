from permute import perm1, perm4, clp_perm, pyorg_perm, xpermutations
from gray_code import GrayCodeIterator
from itertools import permutations
from libpermvis import graph

width = 720
height = 120

ldrawer = graph.Weave(width, height)

def transpose(l):
  return zip(*l)

#perms 1*, 2, 3, pyorg_perm and python's permutation are all
#lexical permutations
for algo in [perm1, perm4, clp_perm, pyorg_perm, xpermutations]:
  perm = transpose(list(i[:] for i in algo(range(4))))
  ldrawer.draw(
    perm,
    "perm", #title
    "%s.png" % algo.__name__, #filename
    6, #line width
    1, #border
    False, #rotate
  )
