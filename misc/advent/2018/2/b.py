from itertools import *


def is_onec(x, y):
    diff = False
    for a, b in zip(x, y):
        if a != b:
            if diff:
                return False
            diff = True
    return True


for linea in open("input.txt"):
    for lineb in open("input.txt"):
        if linea == lineb: continue
        if is_onec(linea, lineb):
            print(f"{linea.strip()}\n{lineb.strip()}\n\n")
