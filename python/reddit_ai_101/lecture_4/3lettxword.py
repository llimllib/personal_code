#!/usr/local/bin/python3

def getwords(dictionary_filename):
    words = set()
    for line in open(dictionary_filename):
        lines = line.split()
        for l in lines: words.add(l.strip())
    return words

class CWRvertex:
    #Each crossword consists of four words around an empty square like so:
    # TOR
    # E#O
    # NOT
    #
    # where the words are numbered:
    # 0: top, across (TOR)
    # 1: right, down (ROT)
    # 2: bottom, across (NOT)
    # 3: left, down (TEN)
    def __init__(self, parent=None):
        self.father = parent 
        self.CWR = parent.CWR[:] if parent else []
        self.depth = parent.depth+1 if parent else 0 
        self.children = []
        if self.father:
            self.father.children.append(self)

    def __str__(self):
        word1 = self.CWR[1] if len(self.CWR) > 1 else "   "
        word2 = self.CWR[2] if len(self.CWR) > 2 else "   "
        word3 = self.CWR[3] if len(self.CWR) > 3 else "   "
        return """{0}
{1}#{2}
{3}
----""".format(self.CWR[0], word3[1], word1[1], word2)

def makegraph():
    words = sorted(getwords("words.txt"))
    for word0 in words:
        c = CWRvertex()
        c.CWR.append(word0)
        for word1 in words:
            if word1[0] == word0[-1]:
                d = CWRvertex(parent=c)
                d.CWR.append(word1)
                for word2 in words:
                    if word2[-1] == word1[-1]:
                        e = CWRvertex(parent=d)
                        e.CWR.append(word2)
                        for word3 in words:
                            if word3[0] == word0[0] and word3[-1] == word2[0]:
                                f = CWRvertex(parent=e)
                                f.CWR.append(word3)
                                print(f)


if __name__=="__main__":
    makegraph()
