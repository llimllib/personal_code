# part 1
print(sum((int(line) // 3) - 2 for line in open("a.txt")))

# part 2
def fueler(mass):
    fuel = (mass // 3) - 2
    if fuel <= 0:
        return 0
    return fuel + fueler(fuel)


print(sum(fueler(int(line)) for line in open("a.txt")))
