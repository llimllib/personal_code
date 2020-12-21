def solve(busids, busidxs, minans):
    i = ((minans // busids[0]) * busids[0]) - busidxs[0]
    p = i
    bump = busids[0]
    for idx, busid in enumerate(busids[1:]):
      while 1:
        if (i - (busid - busidxs[idx + 1])) % busid == 0:
          bump = bump * busid
          break
        else:
          i += bump
    return i
        

mark = solve([19,41,37,367,13,17,29,373,23], [0,1,2,3,4,5,6,7,8], 1)
bill = solve(
    [883, 19, 37, 23, 13, 17, 797, 41, 29],
    [19, 0, 13, 27, 32, 36, 50, 60, 79],
    100000000000000,
)
print((bill-100000000000000)/(mark-100000000000000))
