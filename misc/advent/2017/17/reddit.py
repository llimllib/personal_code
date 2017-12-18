from collections import deque

puzzle = 312
spinlock = deque([0])

for i in range(1, 50000001):
    spinlock.rotate(-puzzle)
    spinlock.append(i)

print(spinlock[spinlock.index(0) + 1])
