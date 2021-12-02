# PYTHONPATH is modified locally to include the top-level utils package,
# because python paths are annoying as heck
from utils import iterpair, itertrip, openints


def n_increase(it):
    return sum(a < b for a, b in it)


print(
    n_increase(
        iterpair(openints("sample.txt")),
    )
)
print(
    n_increase(
        iterpair(openints("input.txt")),
    )
)


print(n_increase(iterpair(map(sum, itertrip(openints("sample.txt"))))))

print(n_increase(iterpair(map(sum, itertrip(map(int, open("input.txt")))))))
