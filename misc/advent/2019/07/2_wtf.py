import sys
import itertools
import ipdb

from cpu3 import cpu


mem = [int(n) for n in open("test3").read().strip().split(",")]
mem1, output, ip1 = cpu(mem[:], [9, 0])
print(output)
mem2, output, ip2 = cpu(mem[:], [8, int(output)])
print(output)
mem3, output, ip3 = cpu(mem[:], [7, int(output)])
print(output)
mem4, output, ip4 = cpu(mem[:], [6, int(output)])
print(output)
mem5, output, ip5 = cpu(mem[:], [5, int(output)])
print(output)
while output:
    mem1, output, ip1 = cpu(mem1, [int(output)], ip=ip1)
    print("-> ", output)
    mem2, output, ip2 = cpu(mem2, [int(output)], ip=ip2)
    print("-> ", output)
    mem3, output, ip3 = cpu(mem3, [int(output)], ip=ip3)
    print("-> ", output)
    mem4, output, ip4 = cpu(mem4, [int(output)], ip=ip4)
    print("-> ", output)
    mem5, output, ip5 = cpu(mem5, [int(output)], ip=ip5)
    print("-> ", output)
