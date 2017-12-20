from collections import defaultdict
import re
import math


def manh(particle):
    return math.sqrt(particle[0]**2 + particle[1]**2 + particle[2]**2)


def go(inp):
    particles = []
    for i, line in enumerate(inp):
        particle = list(map(int, re.findall(r'[\-\d]+', line)))
        particle.append(i)
        particles.append(particle)

    # randomly guessed 1000 loops would be "enough"
    for i in range(1000):
        positions = defaultdict(list)
        for particle in particles:
            particle[3] += particle[6]
            particle[4] += particle[7]
            particle[5] += particle[8]
            particle[0] += particle[3]
            particle[1] += particle[4]
            particle[2] += particle[5]
            p = (particle[0], particle[1], particle[2])
            positions[p].append(particle)
        for dupicles in positions.values():
            if len(dupicles) > 1:
                for dupicle in dupicles:
                    particles.remove(dupicle)

    print(len(particles))
    ds = [(manh(p), p) for p in particles]
    print(list(sorted(ds))[0])


if __name__ == "__main__":
    go(open("input.txt"))
