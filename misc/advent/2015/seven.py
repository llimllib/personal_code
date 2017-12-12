import re

lines = open("input.txt").readlines()

ns = {}

cmds = []
for line in lines:
    cmd, name = line.split(' -> ')
    cmds.append((cmd.split(' '), name.strip()))

def get(var):
    if var.isdigit(): return int(var)
    if var in ns: return ns[var]

def exe(cmd, name):
    # assignment
    if len(cmd) == 1:
        val = get(cmd[0])
        if val is not None:
            ns[name] = val
        return
    if cmd[0] == "NOT":
        val = get(cmd[1])
        if val is not None:
            ns[name] = ~val
        return
    else:
        a = get(cmd[0])
        b = get(cmd[2])
        if a is not None and b is not None:
            if cmd[1] == "AND":
                ns[name] = a & b
            elif cmd[1] == "OR":
                ns[name] = a | b
            elif cmd[1] == "RSHIFT":
                ns[name] = a >> b
            elif cmd[1] == "LSHIFT":
                ns[name] = a << b
            else:
                raise Exception('wassup {}'.format(cmd))
        return

ll = len(ns)
while 'a' not in ns:
    for cmd, name in cmds:
        if name not in ns:
            try:
                exe(cmd, name)
            except:
                print(cmd, name, ns)
                raise
    if len(ns) == ll:
        raise Exception("no progress {}".format(ns))
    ll = len(ns)
print(ns['a'])

ns = {'b': ns['a']}
while 'a' not in ns:
    for cmd, name in cmds:
        if name not in ns:
            try:
                exe(cmd, name)
            except:
                print(cmd, name, ns)
                raise
    if len(ns) == ll:
        raise Exception("no progress {}".format(ns))
    ll = len(ns)
print(ns['a'])
