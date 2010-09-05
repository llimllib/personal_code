#!/usr/local/bin/python3
from copy import copy
import re

def getwords(dictionary_filename):
    return {w for w in open(dictionary_filename).read().split()}

#the problem: given a crossword spec to be filled out with 3-letter words, print out
# all possible solutions.
# the previous problem would be the case where the spec looked like this:
# ...
# .#.
# ...
#
# But we could have more complicated cases like:
#
# ...#...
# .#...#.
# .#.#.#.
class Xword:
    def __init__(self):
        self.nextword = 0

    def setspec(self, spec):
        self.spec = spec
        self.mtx = [list(x) for x in spec.split()]
        #an array of ((x1,y1),(x2,y2)) word definitions
        self.words = self.getwords()

    def getwords(self):
        rowlen = len(self.mtx)
        cols = list(zip(*self.mtx))
        words = []
        for i, row in enumerate(self.mtx):
            for j, letter in enumerate(self.mtx[i]):
                if letter != "#":
                    if j < len(row)-1 and self.mtx[i][j+1] != '#' \
                    and (j==0 or self.mtx[i][j-1] == '#'):
                        #we have an across word, find the end and add it to words
                        try:
                            end = row.index('#', j)
                        except ValueError:
                            end = len(row)
                        words.append(((j,i),(end-1,i)))
                    if i < rowlen-1 and self.mtx[i+1][j] != '#' \
                    and (i==0 or self.mtx[i-1][j] == '#'):
                        try:
                            end = cols[j].index('#', i)
                        except ValueError:
                            end = len(cols[j])
                        words.append(((j,i),(j,end-1)))
        return words

    def getspec(self):
        return "\n".join("".join(r) for r in self.mtx)

    def getword(self, word):
        (x1, y1), (x2, y2) = word
        #if it's a down word
        if x1 == x2:
            return "".join(list(zip(*self.mtx))[x1][y1:y2+1])
        else:
            return "".join(self.mtx[y1][x1:x2+1])

    def insert(self, word, s):
        (x1, y1), (x2, y2) = word
        letters = list(s)
        print("inserting letters {0}".format(s))
        if x1 == x2:
            for i in range(y1, y2+1):
                letter = letters[i-y1]
                print ("inserting {letter} into {i},{x1}. {y1}".format(**locals()))
                self.mtx[i][x1] = letters[i-y1]
        else:
            for i in range(x1, x2+1):
                letter = letters[i-x1]
                print ("inserting {letter} into {y1},{i}. {x1}".format(**locals()))
                self.mtx[y1][i] = letters[i-x1]

    def getnextword(self):
        while self.getword(self.words[self.nextword]).find('.') == -1:
            self.nextword += 1
        return self.words[self.nextword]

    def copy(self):
        x = Xword()
        x.spec = self.spec
        x.mtx = copy(self.mtx)
        x.words = copy(self.words)
        x.nextword = self.nextword
        return x

    def complete(self):
        return "".join("".join(x) for x in self.mtx).find('.') == -1

    def __str__(self):
        return "\n".join("".join(x) for x in self.mtx) + "\n--------------"

class Node:
    def __init__(self, word, xword):
        self.xword = xword
        self.word = word
        self.xword.insert(self.xword.getnextword(), word)
        print(self.xword)

    def complete(self): return self.xword.complete()

    def fits(self, word):
        if re.match(self.xword.getword(self.xword.getnextword()), word):
            return True
        return False


def makegraph(words, spec):
    for word in words:
        xword = Xword()
        xword.setspec(spec)
        root = Node(word, xword)
        _makegraph(words, root)

def _makegraph(words, root):
    for word in words:
        if root.fits(word):
            n = Node(word, root.xword.copy())
            if n.complete():
                print(n)
            else:
                _makegraph(words, n)
        
if __name__=="__main__":
    x = Xword()
    x.setspec("""...
...
...""")
    print(x.words)
    print(x.getspec())

    x = Xword()
    spec = """...#...
.#...#.
.#.#.#."""
    x.setspec(spec)
    print(x.words)
    print(x.getspec())

    makegraph(getwords("words.txt"), spec)
