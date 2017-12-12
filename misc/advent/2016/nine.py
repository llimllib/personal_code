import re

def read(s):
    fp = 0
    out = []
    replen = 0
    repcount = 0
    reps = 0
    while fp < len(s):
        if s[fp] == '(' and repcount<=0:
            end = s.find(')', fp)
            repcount, reps = map(int, s[fp+1:end].split('x'))
            replen = repcount
            fp = end+1
        else:
            out.append(s[fp])
            repcount -= 1
            if repcount == 0:
                rep = out[-replen:]
                out = out[:-replen] + (rep * reps)
            fp += 1

    return ''.join(out)

def read2(s):
    fp = 0
    n = 0
    while fp < len(s):
        if s[fp] == '(':
            end = s.find(')', fp)
            repcount, reps = map(int, s[fp+1:end].split('x'))
            n += reps * read2(s[end+1:end+repcount+1])
            fp = end + repcount + 1
        else:
            n += 1
            fp += 1
    return n

repeat = re.compile(r'\((\d+)x(\d+)\)')
def read3(s):
    m = repeat.search(s)
    if not m: return len(s)
    else:     return m.start() + int(m.group(2)) * int(m.group(1)) + read3(s[m.end()+int(m.group(1)):])

def read4(s):
    m = repeat.search(s)
    if not m: return len(s)
    else:     return m.start() + int(m.group(2)) * read4(s[m.end():m.end()+int(m.group(1))]) + read4(s[m.end()+int(m.group(1)):])


#tests = [
#    ('ADVENT', 'ADVENT'),
#    ('A(1x5)BC', 'ABBBBBC'),
#    ('(3x3)XYZ', 'XYZXYZXYZ'),
#    ('A(2x2)BCD(2x2)EFG', 'ABCBCDEFEFG'),
#    ('(6x1)(1x3)A', '(1x3)A'),
#    ('X(8x2)(3x3)ABCY', 'X(3x3)ABC(3x3)ABCY'),
#]
#for inp, expected in tests:
#    got = read(inp)
#    if got != expected:
#        raise Exception("{} {} {}".format(inp, expected, got))

f = open("input9.txt").read().strip()
print(len(read(f)))
print(read3(f))

tests = [
    ('(3x3)XYZ', 9),
    ('X(8x2)(3x3)ABCY', 20),
    ('(27x12)(20x12)(13x14)(7x10)(1x12)A', 241920),
    ('(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN', 445),
]
for inp, expected in tests:
    got = read4(inp)
    if got != expected:
        raise Exception("{} {} {}".format(inp, expected, got))
print(read2(f))
print(read4(f))
