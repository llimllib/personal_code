import team_paths

fout = file('illinois.html', 'w')
fout.write('<html><head><title>Illinois Numbers</title></head>\n')
fout.write('<body><form action="illinois.py" method="GET">\n')
fout.write('Find the Illinois number of:<br>\n')
fout.write('<select name="team">\n')
tp = team_paths.team_paths
for key in tp.iterkeys():
    if tp[key].find('does not have') == -1:
        fout.write('<option value="%s">%s\n' % (key, key.title()))
fout.write('</select></form></body></html>')
