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

@test
@param(known_values)
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

class testing123:
    @test
    def wtf():
        print "yyyo"

for i in test_to_roman():
    #how in the world is this supposed to be used?
    i[0](i[1], i[2].upper())
print "passed"
