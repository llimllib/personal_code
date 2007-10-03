import os

class reversefile(object):
    """Iterate backwards through a file. f should be an open file handle"""
    def __init__(self, f):
        self._f = f
        self.end = os.stat(f.name).st_size

    def __iter__(self): return self

    def next(self):
        if self.end == 0:
            raise StopIteration

        pos = self.end-2
        while pos >= 0:
            self._f.seek(pos)
            if self._f.read(1) == '\n':
                end = self.end
                self.end = pos
                return self._f.read(end - pos - 1)
            pos -= 1

        end = self.end
        self.end = 0
        self._f.seek(0)
        return self._f.read(end).strip()

def test_reversefile():
    tests = [
"",
"\n",
"something\n",
"other",
"""alpha
beta
gamma""",
"""something with lots of long lines could sometimes mess it up?
Who knows but I'm going to need some test text that's for sure.
What edge conditions should I be testing? This implementation leaves
no bufsize borders to test; that's a pretty nice property. I guess that
I can test with an empty string and a bare \n. I'm going to rely on tests
without manually compiled answers, because this is a pretty basic function."""
    ]

    for t in tests:
        print "testing: <%s>" % t

        #write out the file for testing
        testfile = file("deleteme.tmp", "w")
        testfile.write(t)
        testfile.close()

        expected = list(reversed(
            [l.strip('\n') for l in file("deleteme.tmp").readlines()]))

        got = list(reversefile(file("deleteme.tmp")))

        if got != expected:
            raise """failure with 
%s
=======================
%s""" % (expected, got)

if __name__ == "__main__":
    test_reversefile()
