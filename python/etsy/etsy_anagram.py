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
    return sorted(list(w1)) == sorted(list(w2))

if __name__ == "__main__":
    tests = [("test", "sett", True),
        ("pot", "top", True),
        ("", "", True),
        ("longword", "drowgnol", True),
        ("alpha", "beta", False),
        ("", "a", False),
        ("a", "", False),
        ("test", "set", False)] #failed on previous version
    
    for w1, w2, expected in tests:
        assert(isAnagram(w1, w2) == expected)
        print ".",
