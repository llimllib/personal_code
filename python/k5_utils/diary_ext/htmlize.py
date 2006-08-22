def htmlize(fin, fout):
    for line in fin:
        line = line.replace(' ', '&nbsp;')
        line = line.replace('<', '&lt;')
        line = line.replace('>', '&gt;')
        line += '<br>'
        print >>fout, line

if __name__ == "__main__":
    fin = file('diary_ext.py')
    fout = file('out.html', 'w')
    htmlize(fin, fout)
