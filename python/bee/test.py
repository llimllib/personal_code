test = frozenset("equivoke")
# test = frozenset("cazique")
n = 0
for word in set(line.strip() for line in open("words.txt") if len(line.strip()) > 3):
    if test.issuperset(frozenset(word)):
        n += 1
        print(word)
print(n)


red = "\033[0;31m"
green = "\033[0;32m"
blue = "\033[0;34m"
reset = "\033[0m"
print(f"{green}words\tpangram")
for n, pangram in [(1, "bananas"), (2, "test")]:
    print(f"{blue}{n}{reset}\t{pangram}")
