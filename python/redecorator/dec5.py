import re

#######
# convenient regular expressions

decodef = re.compile("^\s*\@(\S*)")
funcdef = re.compile("^\s*def\s*(\S*)\s*\(")
comdef = re.compile("^\s*\#")

#######
# decorated file object

class deco1(object):

   def __init__(self,fnam):
      self.raw = file(fnam).readlines()
      self.ofnam = fnam.replace("ppy","py")
      self.final = munger(self.raw)[0]
      
   def save(self):
      outf = file(self.ofnam,"w")
      print >> outf, ''.join(self.final)
      outf.close()
      print "wrote " + self.ofnam

#######
# gets python indentation

def leadblanks(line):
      """finds python indentation of a line"""
      blanks = 0
      for char in line:
         if char == " ":
            blanks += 1
         elif char == "\t":
            blanks += (blanks % 8) and (8-blanks%8) or 8
         else:
            break
      return blanks

#######
# extracts decorator name

def deconame(line):
   return decodef.match(line).group(1).split("#")[0]

#######
# extracts function name

def funcname(line):
   return funcdef.match(line).group(1)

#######
# recursively process text

def munger(inlist,state="default"):
   """ start here
   """
   outlist = []
   count = 0
   oldstate = state
   blanks = 0
   dfuncs = []
   fname = ""
   while count < len(inlist):
      line = inlist[count]
      state = nextstate(state,line,blanks)
      if state == "gotdeco":
         a = munger(inlist[count:],"indeco")
         count = count + a[1]
         outlist.extend(a[0])
         state = oldstate
      elif state == "indeco" and decodef.match(line):
         dfuncs.append(deconame(line))
         count += 1
      elif state == "enterfunc":
         fname = funcname(line)
         blanks = leadblanks(line)
         outlist.append(line)
         state = "infunc"
         count += 1
      elif state == "infunc":
         outlist.append(line)
         count += 1
      elif state == "exitfunc":
         outlist.append(blanks * " " + fname + " = " + "(".join(dfuncs) + "(" + fname + len(dfuncs) * ")"+"\n")
         return (outlist,count)
      elif state == "default":
         outlist.append(line)
         count += 1
      else:
         raise NotImplementedError,"Bogus state in munger(): %s\n" % state
   if state == "gotdeco":
      raise ValueError,"unfinished decorator"
   else:
      if fname:
         outlist.append(blanks * " " + fname + " = " + "(".join(dfuncs) + "(" + fname + len(dfuncs) * ")" + "\n")
      return(outlist,count)

####### 
# finite state machine processes lines to determine what to do with them

def nextstate(state,line,blanks=0):
   """ state machine to parse decoration-extended python 
   """

   if state == "default":
      if decodef.match(line):
         state = "gotdeco"
      else:
         state = "default"
   elif state == "indeco":
      if funcdef.match(line):
         state = "enterfunc"
      elif decodef.match(line) or comdef.match(line) or line.isspace():
         pass
      else:
         raise ValueError,"invalid line in decorator definition: " + line
   elif state == "infunc":
      if blanks >= leadblanks(line):
        state = "exitfunc"
      else:
        state = "infunc"
   elif state == "enterfunc":
      raise NotImplementedError,"caller must transit from enterfunc to infunc"
   elif state == "gotdeco":
      raise NotImplementedError,"caller must transit from gotdeco to indeco"
   elif state == "infunc":
      if leadblanks(line) <= blanks:
         state = "exitfunc"
      elif decodef.match(line):
         state = "gotdeco"
   else:
      raise ValueError,"bogus state"
   return(state)

#######
# run the preprocessor only where needed
#
# would be better if this could wrap import, but apparently not possible

def PreImport(thing):
   import sys
   from os.path import isfile
   from os import stat

   infnam = str(thing) + ".ppy"
   ofnam1 = str(thing) + ".py"
   ofnam2 = str(thing) + ".pyc"
   found = False
   mod1 = mod2 = 0

   for filepath in sys.path:
      if not filepath.isspace:
         filepath = filepath + "/"
      if isfile(filepath + infnam):
         foundpath = filepath
         found = True
         ppmod = stat(foundpath + infnam)[8] 
         break

   if found:
      if isfile(foundpath + ofnam1):
         mod1 = stat(foundpath+ofnam1)[8]
      if isfile(foundpath + ofnam2):
         mod2 = stat(foundpath + ofnam2)[8]

      if max(mod1,mod2) < ppmod:
         c = deco1(foundpath + infnam)
         c.save()

if __name__ == "__main__":
    o, c = munger(file('dec_test.py').readlines())
    of = file('dec5_out.py', 'w')
    for line in o: of.write(line)

######## 
# demo

if __name__ == "__main__t":

   ######################
   # NB this is just data; typically it would already be in a file
   #

   testfile = """
def foo(func):
    def foo2(*args):
       print "foo!"
       func(*args)
    return foo2

@foo
def bazz(thing):
    print "hello,", thing

if __name__=="__main__":
    bazz("mt")

   """

   #
   # end data
   #####################

   # write data to a file

   # the first try will fail on first run,
   # and on subsequent runs will skip the preprocessor 

   try:
      PreImport("tst")
      import tst
      tst.bazz("Miguel")
   except:
      pass

   # write the unpreprocessed file

   testf = open("tst.ppy","w")
   testf.write(testfile)
   testf.close()

   #delete the old module if it's imported

   try:
      del tst
   except:
      pass
   PreImport("tst")

   # the whole 9 yards this time

   import tst
   tst.bazz("Mischka")
