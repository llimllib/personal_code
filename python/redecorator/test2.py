import roman
known_values = [(1, 'i'), (2, 'ii'), (3, 'iii')]

def test(a):
    print a
    return a

def param(param_list):
    def decorator(func):
        def yielder():
            for params in param_list:
                yield (func,) + params
        return yielder
    return decorator

def test_to_roman(integer, 
    numeral):
    """docstring
which starts at the left"""
    d = [1,2,
3, 4]
    print integer, numeral, roman.toRoman(integer)
    print "this line takes 2 %s lines" \
        % 'long'

    assert roman.toRoman(integer) == numeral

test_to_roman = test(param(known_values))(test_to_roman)

class testing123:
    def wtf():
        print "yyyo"
    wtf = test(wtf)


for i in test_to_roman():
    #how in the world is this supposed to be used?
    i[0](i[1], i[2].upper())
print "passed"
