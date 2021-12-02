import io
from itertools import tee, takewhile
import functools


def compose(*functions):
    return functools.reduce(lambda f, g: lambda x: f(g(x)), functions, lambda x: x)


def skip(it):
    next(it)
    return it


def iterpair(it):
    it1, it2 = tee(it)
    return zip(it1, skip(it2))


def itertrip(it):
    it1, it2, it3 = tee(it, 3)
    return zip(it1, skip(it2), skip(skip(it3)))


def openints(fname: io.TextIOBase):
    """
    openints taks the filename of a file containing
    integers separated by newlines and returns an iterator
    of integers

    >>> list(openints("small.txt"))
    [100, 101, 102]
    """
    return map(int, fname)


def strip(x: str) -> str:
    return x.strip()


def split(x: str) -> list[str]:
    return x.split()


def openchars(fname: str):
    """
    openchars returns an iterator over a list of lists of
    characters, with the newlines stripped

    assuming "small.txt" is a file containing "##...###\n..###..."
    >>> list(openchars("small.txt"))
    [['#', '#', '.', '.', '.', '#', '#', '#'],
     ['.', '.', '#', '#', '#', '.', '.', '.']]
    """
    return map(compose(list, strip), open(fname))


def opensplit(fname: str):
    """
    opensplit returns an iterator over lines split on spaces

    assuming "small.txt" is a file containing "foo 1\nbar 2\n"
    >>> list(openchars("small.txt"))
    [['foo', '1'],
     ['bar', '2']]
    """
    return map(compose(list, split), open(fname))


def openlines(fname: str):
    """
    openlines returns an iterator over a list of lines of
    characters, with the newlines stripped

    assuming "small.txt" is a file containing "##...###\n..###..."
    >>> list(openlines("small.txt"))
    [['##...###'],
     ['..###...']]
    """
    return map(strip, open(fname))


def opengroups(fname: str):
    """
    openlines returns an iterator over a list of groups of
    lines of characters separated by pairs of newlines,
    with the trailing newlines stripped

    assuming "small.txt" is a file containing: "a\nb\n\ncd\ne\nf\n\ng"
    >>> list(opengroups("small.txt"))
    [['a' 'b'],
     ['cd', 'e', 'f'],
     ['g']]
    """
    buf = []
    it = open(fname)
    for line in it:
        if line == "\n":
            yield buf
            buf = []
        else:
            buf.append(line.strip())
    yield buf
