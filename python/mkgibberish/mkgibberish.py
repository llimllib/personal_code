import random, os

iter = 3500

dircount = 0
filecount = 0
depth = 0
count = 0
for i in xrange(iter):
    r = random.randint(0,99)
    if r < 10:
        dircount += 1
        d = 'gibberdir' + str(dircount)
        try: os.mkdir(d)
        except OSError: continue  #why do I get "file not found" errors - I'm
                                  #creating a goddamn dir!
        os.chdir(d)
        depth += 1
    elif r < 20:
        if depth > 0:
            os.chdir('..')
            depth -= 1
    else:
        filecount += 1
        fname = 'gibber' + str(filecount) + '.txt'
        f = file(fname, 'w')
        gibberish = 'lorem ipsum set amet dolor motherf$%^er!\n'
        f.write(gibberish * (r*29))
    if i%100 == 0: print "%d iters" % i
print "%d files, %d dirs" % (filecount, dircount)
