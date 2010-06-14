from permute import perm1, perm4, clp_perm, pyorg_perm, xpermutations
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
  l = range(4)
  perms = list(i[:] for i in algo(l[:]))
  perm = [[p.index(i) for p in perms] for i in l]
  ldrawer.draw(
    perm,
    "perm", #title
    "%s.png" % algo.__name__, #filename
    6, #line width
    1, #border
    False, #rotate
  )
