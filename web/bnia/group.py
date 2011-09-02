import cPickle

def group(o, by):
    grps = {}
    if isinstance(o, list):
        for i in o:
            #XXX: is grouping unknown by "None" the right thing to do?
            if isinstance(by, (str, unicode)):
                grp = getattr(i, by, None)
            elif isinstance(by, type(lambda:1)):
                grp = by(i)

            grps.setdefault(grp, []).append(i)

    elif isinstance(o, dict):
        for key, val in o.iteritems():
            grps[key] = group(val, by)

    return grps
