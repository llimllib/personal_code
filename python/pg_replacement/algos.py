import random, Image

###
# global page replacement algorithms
###

class rand_replace:
    def __init__(self, mem, rs):
        self.length = len(mem) - 1
        self.rand = random.Random()

    def replace(self, p): return self.rand.randint(0, self.length)

    def reference(self, p): pass

class min_replace:
    def __init__(self, mem, rs):
        self.rs = rs
        self.mem = mem

    def replace(self, p):
        max_i = 0                               #current largest index value
        for i in range(len(self.mem)):
            try:
                l = self.rs.index(self.mem[i])  #throws ValueError if mem[i]
                                                #does not occur later in the rs
                if l > max_i: max_i = i         #l occurs later in the rs than
                                                #max_i, so replace it
            except ValueError: return i         #the value's not in the rs, so
                                                #return it
        return max_i

    def reference(self, p): pass

class fifo_replace:
    def __init__(self, mem, rs):
        self.length = len(mem)
        self.oldest = 0
    
    def replace(self, p):
        self.oldest = (self.oldest+1) % self.length
        return self.oldest

    def reference(self, p): pass

class lru_replace:
    def __init__(self, mem, rs):
        self.length = len(mem)
        self.q = [-1 for i in range(len(mem))]
        self.mem = mem

    def replace(self, p):
        self.q.insert(0, p)
        return self.mem.index(self.q.pop())    #return the index of the
                                               #last element in the q

    def reference(self, p):
        del self.q[self.q.index(p)]    #delete p
        self.q.insert(0, p)            #and move it to the head of the queue

class second_chance_replace:
    def __init__(self, mem, rs):
        self.length = len(mem)
        self.mem = mem
        self.ptr = 0
        self.bitmap = Image.new('1', (self.length, 1), 1)

    def replace(self, p):
        while 1:
            if self.bitmap.getpixel((self.ptr,0)) == 0:
                self.ptr = (self.ptr+1) % self.length
                return (self.ptr-1) % self.length
            else:
                self.bitmap.putpixel((self.ptr,0), 0)
                self.ptr = (self.ptr+1) % self.length

    def reference(self, p):
        self.bitmap.putpixel((self.mem.index(p),0), 1)

class third_chance_replace:
    """third chance replacement algorithm

    since this is the only algorithm that needs to know if an access was a write
    or a read, I make that determination randomly inside this class. I've given
    a 20% chance of an access being a write"""
    def __init__(self, mem, rs):
        self.length = len(mem)
        self.mem = mem
        self.ptr = 0
        self.use_bmp = Image.new('1', (self.length, 1), 1)
        self.wrt_bmp = Image.new('1', (self.length, 1), 1)
        self.rand = random.Random()
        self.write_chance = .2

    def replace(self, p):
        while 1:
            used = self.use_bmp.getpixel((self.ptr, 0))
            if not used and not self.wrt_bmp.getpixel((self.ptr, 0)):
                self.ptr = (self.ptr+1) % self.length
                return (self.ptr-1) % self.length
            else:
                if not used: self.wrt_bmp.putpixel((self.ptr, 0), 0)
                self.use_bmp.putpixel((self.ptr, 0), 0)
                self.ptr = (self.ptr+1) % self.length

    def reference(self, p):
        if self.rand.random() < self.write_chance: write = 1
        else: write = 0
        idx = self.mem.index(p)
        if write: self.wrt_bmp.putpixel((idx,0), 1)
        self.use_bmp.putpixel((idx,0), 1)

###
# local page replacement algorithms
###

class vmin_local_replace:
    """Local min replacement.

    At any given time, the resident set consists of those pages that will be
    referenced between now and time+self.tau"""
    def __init__(self, ws, rs, tau):
        self.rs = rs
        self.tau = tau
        self.ws = ws
        self.cur_frames = 0
        self.n_accesses = 0
        self.d = file('d.out', 'w')

    def replace(self, p):
        for frame in self.ws:
            if frame != -1 and not self.check_window(frame):
                self.ws[self.ws.index(frame)] = -1
            elif frame != -1: self.cur_frames += 1
        self.cur_frames += 1    #for the p we're inserting now
        self.n_accesses += 1    #so we can calculate average # of page frames
                                #in the working set
        i = self.ws.index(-1)   #find free frame
        self.ws[i] = p    #insert p at the free frame
        return i

    def reference(self, p):
        for frame in self.ws:
            if frame != -1 and not self.check_window(frame):
                self.ws[self.ws.index(frame)] = -1
            elif frame != -1: self.cur_frames += 1
        self.n_accesses += 1

    def check_window(self, p):
        if p in self.rs[:self.tau]: return 1
        return 0

class ws_local_replace:
    """Working set local replacement"""
    def __init__(self, ws, rs, tau):
        self.ws = ws
        self.rs = rs
        self.tau = tau
        self.cur_frames = 0
        self.n_accesses = 0
        self.history = []

    def replace(self, p):
        if len(self.history) >= self.tau: x = self.history.pop()
        self.check_window()
        self.history.insert(0, p)
        return self.ws.index(-1)

    def reference(self, p):
        if len(self.history) >= self.tau: x = self.history.pop()
        self.check_window()
        self.history.insert(0, p)

    def check_window(self):
        self.n_accesses += 1
        for pg in self.ws:
            if pg not in self.history: self.ws[self.ws.index(pg)] = -1
            elif pg != -1: self.cur_frames += 1

class pff_local_replace:
    """Page fault frequency replacement algorithm"""
    def __init__(self, ws, rs, tau):
        self.ws = ws
        self.rs = rs
        self.tau = tau
        self.cur_frames = 0
        self.n_accesses = 0
        self.pf_time = 0
        self.history = []

    def replace(self, p):
        self.n_accesses += 1
        if self.n_accesses - self.pf_time > self.tau:
            self.history = self.history[:self.tau]
            for pg in self.ws:
                if pg not in self.history: self.ws[self.ws.index(pg)] = -1
                elif pg != -1: self.cur_frames += 1
        self.history.insert(0,p)
        try:
            return self.ws.index(-1)
        except ValueError:
            self.ws.append(-1)
            return len(self.ws) - 1

    def reference(self, p):
        self.history.insert(0, p)
        self.n_accesses += 1
