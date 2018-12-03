import sys

freq = 0
found = {0}
print(0)
while 1:
    for line in open("input.txt"):
        if not line.strip(): continue
        n = int(line.strip())
        freq += n
        if freq in found:
            print(f"first double: {freq}")
            sys.exit(0)
        found.add(freq)
        print(freq)
