#!/usr/bin/python
from scheduler import *

# try to import readline; if it works, raw_input gains nifty features. If not,
# just do nothing, and raw_input still works.
try: import readline
except ImportError: pass

def shell(scheduler):
    try:
        while(1):
            line = raw_input('>>>') #get input
            line = line.split(' ')  #split it by whitespace
            if scheduler.has_func(line[0]):
                scheduler.ex(line[0], line[1:])  #call the given function
            elif line[0].lower() == 'quit' or line[0].lower() == 'exit':
                raise EOFError
            else:
                print "no function found with name %s\n" % line[0]
    except EOFError:
        print "Thanks for using the schedule simulator.\n"
    except NotImplementedError:
        print "Function not yet implemented"
    except ValueError:
        print "You gave incorrect arguments for function %s.\n" % line[0]
        shell(scheduler)

#this line means execute this block only if this file is being run on its own,
#as opposed to imported by some other module
if __name__ == "__main__":
    s = Scheduler()
    shell(s)
