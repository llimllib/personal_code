#!/usr/bin/env python
"""
Code for etsy job application.

Simply execute this file to run a few simple tests of the anagram function.

Bill Mill
12/13/07
bill.mill@gmail.com
http://billmill.org
"""

def isAnagram(w1, w2):
    """ return true if w1 and w2 contain exactly the same letters """
    return set(w1) == set(w2)

if __name__ == "__main__":
    tests = [("test", "sett", True),
        ("pot", "top", True),
        ("", "", True),
        ("longword", "drowgnol", True),
        ("alpha", "beta", False),
        ("", "a", False),
        ("a", "", False)]
    
    for w1, w2, expected in tests:
        assert(anagram(w1, w2) == expected)
        print ".",
