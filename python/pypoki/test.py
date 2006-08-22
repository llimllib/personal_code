class test:
    def __init__(self):
        self.string = 'b'

    def __gt__(self, other):
        if ord(self.string[0]) > ord(other.string[0]): return 1
        return 0

x = test()
y = test()
y.string = 'c'
z = test()
z.string = 'a'

print x > y, x > z
