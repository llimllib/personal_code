import re
from math import ceil
import string

# pip install lark
from lark import Lark

rawgrammar = """
0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"
"""


def rulename(n):
    return "".join(
        [string.ascii_lowercase[(n // (i + 1)) % 26] for i in range(ceil((n + 1) / 25))]
    )


def make_lark_grammar(inp):
    # 8: 42 | 42 8
    # 11: 42 31 | 42 11 31
    rulemap = {}
    for line in inp.strip().split("\n"):
        rule, *sub = re.split("[:|]", line)
        if rule == "8":
            sub = ["42", "42 8"]
        if rule == "11":
            sub = ["42 31", "42 11 31"]
        rulemap[rule] = (rulename(int(rule)), [s.strip().split() for s in sub])

    for _, rules in rulemap.values():
        for rulepart in rules:
            for i in range(len(rulepart)):
                if rulepart[i].isdigit():
                    # replace the rule number with its name
                    rulepart[i] = rulemap[rulepart[i]][0]

    grammar = []
    for name, rules in rulemap.values():
        grammar.append(f"{name}: {' | '.join(' '.join(p) for p in rules)}")

    return "\n".join(grammar)


def match(parser, s):
    try:
        parser.parse(s)
        return True
    except:
        return False


grammar = make_lark_grammar(rawgrammar)
parser = Lark(grammar, start="a")

assert match(parser, "ababbb")
assert match(parser, "abbbab")
assert not match(parser, "bababa")
assert not match(parser, "aaabbb")
assert not match(parser, "aaaabbb")

rawgrammar, tests = open("input.txt").read().split("\n\n")
grammar = make_lark_grammar(rawgrammar)
print(grammar)
parser = Lark(grammar, start="a")
passed = 0
for test in tests.split("\n"):
    if match(parser, test):
        passed += 1
print(passed)
