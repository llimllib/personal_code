import os, stat

class reversefile(object):
"""Iterate backwards through a file. f should be an open file handle"""
    def __init__(self, f):
        self._f = f
        self.end = os.stat(f.name)
        self.linecache = []

    def __iter__(self): return self

    def next(self):
        if len(self.linecache) > 0:
            return self.linecache.pop()

        if self.end == 0:
            raise StopIteration


