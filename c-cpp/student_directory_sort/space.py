#!/usr/bin/python

def space(f):
    lines = f.readlines()
    out_lines = []
    i = 0
    for line in lines:
        out_lines.append([])
        while 1:
            quote = line.find('"')
            comma = line.find(',')
            if quote > comma and comma != -1:
                out_lines[i].append(line[:comma])
                line = line[comma+1:]
            elif quote < comma:
                quote2 = line.find('"', quote + 1)
                out_lines[i].append(line[quote+1:quote2])
                line = line[quote2+2:]
            else:
                out_lines[i].append(line.strip('"\n'))
                break
        i += 1
    return out_lines

def prepare_output(a):
    longest = []
    msc = 1500
    for i in range(len(a[0])):
        #assumes all rows have same width
        longest.append(0)
    for i in range(len(a)):
        try: int(a[i][2])
        except ValueError:
            if i > 0:
                a[i][2] = str(msc)
                msc += 1
        for j in range(len(a[i])):
            if len(a[i][j]) > longest[j]:
                longest[j] = len(a[i][j])
    for i in range(len(a)):
        for j in range(len(a[i])):
            a[i][j] = a[i][j].ljust(longest[j] + 1)
    return a

if __name__ == "__main__":
    in_f = file('fall.csv', 'r')
    out_f = file('fall.prn', 'w')
    a = space(in_f)
    a = prepare_output(a)
    for i in range(len(a)):
        for j in range(len(a[i])):
            out_f.write(a[i][j])
        out_f.write('\n')
