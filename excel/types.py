import re

regex=re.compile(r'^\s+([A-Z]+) = ("[A-Z\s]+?")')
regex=re.compile(r'^\s+([A-Z]+) = ("[A-Za-z()\- ]+")')

f = open('types.txt', 'r')
out = open('names.txt', 'w')
matches = []
line = f.readline()
while line:
	r = regex.match(line)
	if r:
		matches.append((r.group(1), r.group(2)))
	line = f.readline()
for key, match in matches:
	out.write("allTypes.Add Key:=\"%s\", Item:=%s\n" % (key, match))
