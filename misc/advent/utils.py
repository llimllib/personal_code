from itertools import tee
from typing import Iterator, Tuple, TypeVar, Callable

T = TypeVar("T")


# attempting to type this properly is for the insane
def compose(f: Callable, g: Callable) -> Callable:
    return lambda *args: f(g(*args))


def skip(it: Iterator[T]) -> Iterator[T]:
    next(it)
    return it


def iterpair(it: Iterator) -> Iterator[Tuple[Iterator, Iterator]]:
    it1, it2 = tee(it)
    return zip(it1, skip(it2))


def itertrip(it) -> Iterator[Tuple[Iterator, Iterator, Iterator]]:
    it1, it2, it3 = tee(it, 3)
    return zip(it1, skip(it2), skip(skip(it3)))


def strip(x: str) -> str:
    return x.strip()


def split(x: str) -> list[str]:
    return x.split()


def openints(fname: str) -> Iterator[int]:
    """
    openints taks the filename of a file containing
    integers separated by newlines and returns an iterator
    of integers

    >>> list(openints("small.txt"))
    [100, 101, 102]
    """
    return map(int, open(fname))


def openchars(fname: str) -> Iterator[list[str]]:
    """
    openchars returns an iterator over a list of lists of
    characters, with the newlines stripped

    assuming "small.txt" is a file containing "##...###\n..###..."
    >>> list(openchars("small.txt"))
    [['#', '#', '.', '.', '.', '#', '#', '#'],
     ['.', '.', '#', '#', '#', '.', '.', '.']]
    """
    return map(compose(list, strip), open(fname))


def openbits(fname: str) -> Iterator[list[str]]:
    """
    openbits returns an iterator over a list of lists of
    numbers

    assuming "small.txt" is a file containing "1011\n0110"
    >>> list(map(list, openchars("small.txt")))
    [[1, 0, 1, 1],
     [0, 1, 1, 0]]
    """
    return map(compose(lambda x: map(int, x), compose(list, strip)), open(fname))


def opensplit(fname: str) -> Iterator[list[str]]:
    """
    opensplit returns an iterator over lines split on spaces

    assuming "small.txt" is a file containing "foo 1\nbar 2\n"
    >>> list(openchars("small.txt"))
    [['foo', '1'],
     ['bar', '2']]
    """
    return map(compose(list, split), open(fname))


def openlines(fname: str) -> Iterator[str]:
    """
    openlines returns an iterator over a list of lines of
    characters, with the newlines stripped

    assuming "small.txt" is a file containing "##...###\n..###..."
    >>> list(openlines("small.txt"))
    ['##...###',
     '..###...']
    """
    return map(strip, open(fname))


# https://pypi.org/project/more-itertools/ is helpful
def opengroups(fname: str) -> Iterator[list[str]]:
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
