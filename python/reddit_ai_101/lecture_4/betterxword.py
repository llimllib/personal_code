#!/usr/local/bin/python3

def getwords(dictionary_filename):
    return {w for w in open("words.txt").read().split()}

class Node:
    def __init__(self, word, parent=None):
        self.parent = parent
        self.words  = parent.words[:] if parent else []
        self.words.append(word)

    def complete(self): return len(self.words) == 4

    def fits(self, word):
        if not self.words: return True

        l = len(self.words)
        w = self.words
        return ((l == 1 and w[0][2] == word[0]) or
                (l == 2 and w[1][2] == word[2]) or
                (l == 3 and w[0][0] == word[0] and w[2][0] == word[2]))

    def __str__(self):
        word1 = self.words[1] if len(self.words) > 1 else "   "
        word2 = self.words[2] if len(self.words) > 2 else "   "
        word3 = self.words[3] if len(self.words) > 3 else "   "
        return """{0}
{1}#{2}
{3}
----""".format(self.words[0], word3[1], word1[1], word2)

def makegraph(words, root=None):
    for word in words:
        if not root:
            root = Node(word)
        if root.fits(word):
            n = Node(word, root)
            if n.complete():
                print(n)
            else:
                makegraph(words, n)

if __name__ == "__main__":
    makegraph(getwords("words.txt"))
