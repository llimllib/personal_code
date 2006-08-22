#!/usr/bin/env python2.4
"""This program takes one python source file on stdin, and outputs a 
functionally equivalent, but undecorated one to stdout. I run it like:
    ./redecorate.py < dec_test.py > test_out.py

This code is not production code. Run it at your own risk. It will probably 
break your code. Do whatever you want to it; it is in the public domain. For 
questions, comments, or flames, email the author, Bill Mill, at 
bill.mill@gmail.com .

Don't use decorators! They suck!

updated 12-24-04: Fixed handling of multiple decorators (see decendstr). Why
    in the world was I working on this on Christmas Eve? I'm a total addict.
"""
import re, sys, os

DEC_RE = re.compile('^(\s*)@([\w\(\)\, \n]+)%s' % os.linesep)
FUNC_RE = re.compile('^\s*def ([\w\(\)\, \n]+):', re.M)
SPACE_RE = re.compile('^(\s*)')

parens = {'{': 0, '"': 0, '[': 0,  '(': 0}

def notopen():
    global parens
    for key in parens:
        if parens[key] != 0: return None
    return True

def readwrline(fin, fout):
    l = fin.readline()
    fout.write(l)
    return l

def updateparens(line):
    global parens
    pairs = (('{', '}'), ('[', ']'), ('(', ')'))
    for p in pairs:
        i = line.find(p[0])
        j = line.find(p[1])
        if i != -1 and (i == 0 or not line[i-1] == '\\'):
            parens[p[0]] += 1
        if j != -1 and (j == 0 or not line[j-1] == '\\'):
            parens[p[0]] -= 1
    parens['"'] = (parens['"'] + line.count('"')) % 2

def redecorate(fin, fout):
    line = fin.readline()
    while line:
        dec = []
        m = DEC_RE.search(line)
        if not m:
            fout.write(line)
            line = fin.readline()
        else:
            space, decname = m.groups()
            dec.append(decname)
            #check for multiple decorators
            line = fin.readline()
            m = DEC_RE.search(line)
            while m:
                space, decname = m.groups()
                dec.insert(0, decname)
                line = fin.readline()
                m = DEC_RE.search(line)
            #find the function declaration (ignore blank lines btw 
            #decorators and function) and write it out
            fout.write(line)
            func = ''
            m = FUNC_RE.search(line)
            if m:
                func = m.groups()[0]
            while not func:
                line = ''.join((line, readwrline(fin, fout)))
                m = FUNC_RE.search(line)
                func = m.groups()[0]
            func = func[:func.find('(')]
            #now find a line of equivalent indentation to the first
            line = fin.readline()
            m = SPACE_RE.search(line)
            while line:
                cur_spaces = m.groups()[0]
                #This line's more indented, it's not our spot
                if m and len(cur_spaces) > len(space):
                    updateparens(line)
                    fout.write(line)
                #We've got our spot
                elif m and notopen():
                    decstring = ''
                    decendstr = ''
                    for d in dec:
                        if not decstring:
                            decstring = d
                        else:
                            decstring = '%s(%s' % (d, decstring)
                            decendstr = '%s)' % (decendstr)
                    decstring = decstring.rstrip('\n')
                    fout.write('%s%s = %s(%s)%s%s%s%s' % \
                        (space, func, decstring, func, decendstr, os.linesep, 
                        os.linesep, line))
                    line = fin.readline()
                    break
                #We're inside a multi-line statement
                else:
                    updateparens(line)
                    fout.write(line)
                line = fin.readline()
                m = SPACE_RE.search(line)

if __name__ == '__main__':
    try:
        if sys.argv[0].lower().startswith('python'):
            sys.argv = sys.argv[1:] #TODO: test this
        if len(sys.argv) < 2:
            infile = sys.stdin
        else:
            infile = file(sys.argv[1])
        if len(sys.argv) < 3:
            outfile = sys.stdout
        else:
            outfile = file(sys.argv[2], 'w')
        redecorate(infile, outfile)
    except IndexError:
        print "Usage: redecorate.py [<infile> [<outfile>]]"
