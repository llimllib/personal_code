triangles = [map(int, x.strip().split()) for x in open('triangles.txt').readlines()]

i = 0
valid = 0
while i+3 <= len(triangles):
    group = triangles[i:i+3]
    transpose = [sorted(x) for x in zip(*group)]
    for triangle in transpose:
        a,b,c = triangle
        if a + b > c: valid += 1
    i += 3

print(valid)
