from lark import Lark, Transformer, v_args


calc_grammar = """
    ?start: sum

    ?sum: product
        | sum "*" product   -> mul

    ?product: atom
        | product "+" atom  -> add

    ?atom: NUMBER           -> number
         | "(" sum ")"

    %import common.CNAME -> NAME
    %import common.NUMBER
    %import common.WS_INLINE

    %ignore WS_INLINE
"""


@v_args(inline=True)  # Affects the signatures of the methods
class CalculateTree(Transformer):
    from operator import add, sub, mul, truediv as div, neg

    number = int


calc_parser = Lark(calc_grammar, parser="lalr", transformer=CalculateTree())
evil = calc_parser.parse


assert evil("1 + (2 * 3) + (4 * (5 + 6))") == 51
assert evil("2 * 3 + (4 * 5)") == 46
assert evil("5 + (8 * 3 + 9 + 3 * 4 * 3)") == 1445
assert evil("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") == 669060
assert evil("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") == 23340

print(sum(evil(line.strip()) for line in open("input.txt")))
