def mysplit(string, delims):
        matches = []
        s = 0
        for e in range(len(string)):
            if string[e] in delims:
                matches.append(string[s:e])
                s = e+1
        if s < len(string): matches.append(string[s:])
        return matches
