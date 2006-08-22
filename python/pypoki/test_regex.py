def test_re(s, r):
    r = re.compile(r)
    x = r.match(s)
    try: print x.groups()
    except: print "no match"
