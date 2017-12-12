from collections import defaultdict, Counter

signal = open('signal.txt').readlines()
msg = defaultdict(Counter)
for line in signal:
    for i, char in enumerate(line.strip()):
        msg[i].update(char)

def most(c): return c.most_common()[0][0]
def least(c): return c.most_common()[-1][0]

print(''.join(c for _, c in sorted((k, most(v))  for k,v in msg.items())))
print(''.join(c for _, c in sorted((k, least(v)) for k,v in msg.items())))

# one-liners:
print(''.join(Counter(c).most_common(1)[0][0] for c in zip(*list(open('signal.txt').readlines()))))
print(''.join(Counter(c).most_common()[-1][0] for c in zip(*list(open('signal.txt').readlines()))))
