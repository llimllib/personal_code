from collections import Counter, defaultdict
from dateutil.parser import parse
import re


def itermins(m1, m2):
    if m1 < m2:
        for i in range(m1, m2):
            yield i
    else:
        for i in range(m1, 60):
            yield i
        for i in range(0, m2):
            yield i


def maxval(d):
    """Return the dictionary item with the highest value"""
    return max(d.items(), key=lambda x: x[1])


def do(f):
    evts = []
    for line in f:
        date, act = line.split(']')
        date = parse(date.strip('['))
        evts.append((date, act))

    mins = defaultdict(Counter)
    guard = None
    for dt, act in sorted(evts):
        try:
            if 'Guard' in act:
                guard = int(re.search(r'#(\d+)', act).group(1))
            if 'falls' in act:
                sleepstart = dt
            if 'wakes' in act:
                for minute in itermins(sleepstart.minute, dt.minute):
                    mins[guard][minute] += 1
        except:
            print(act)
            raise

    sleepiest_minutes, sleepiest = max(
        (sum(minutes.values()), guard) for guard, minutes in mins.items())
    maxminute, _ = maxval(mins[sleepiest])

    max_ = 0
    longest_guard = None
    minute = None
    for guard, minutes in mins.items():
        min, count = maxval(minutes)
        if count > max_:
            print(f"{guard} {min} {count}")
            max_ = count
            longest_guard = guard
            minute = min
    print(mins[longest_guard])

    print(
        f"A: The sleepiest guard is {sleepiest} {sleepiest_minutes} {maxminute} {sleepiest * maxminute}"
    )
    print(
        f"B: The most minutes asleep was {max_} by {longest_guard} in minute {minute} {longest_guard * minute}"
    )


if __name__ == "__main__":
    do(open("small.txt"))
    do(open("input.txt"))
