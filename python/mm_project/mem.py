import Image, sys

MEM_SIZE = 64
MEMORY = ['e' for i in range(MEM_SIZE)] #our actual fake memory
#PCB = {}            #a map of start location and program size (for removal)

class BlockTooLarge(Exception): pass

class mem:
    def __init__(self, pcb):
        global MEM_SIZE
        self.pcb = pcb
        self.map = Image.new('1', (MEM_SIZE, 1), 0) #our memory bitmap
        self.next = 0
        self.holes = 0
        self.req_counter = 0

    def print_memory(self):
        for i in range(MEM_SIZE):
            if self.map.getpixel((i,0)): sys.stdout.write('O')
            else: sys.stdout.write('e')
        print #print "\n%d" % id(self.pcb)

    def find_hole(self, start, stop, size):
        holes_ex = 0
        for x in range(start, stop):
            if not self.map.getpixel((x, 0)):
                self.holes += 1
                open = 1
                for y in range(1, size):
                    if x+y >= MEM_SIZE or self.map.getpixel((x+y, 0)):
                        #the hole's not big enough
                        open = 0
                        break
                if open:
                    #allocate memory, put the size and location in the pcb 
                    for i in range(size): self.map.putpixel((x+i, 0), 1)
                    self.pcb[x] = size
                    return x
        raise BlockTooLarge

    def get_util(self): return float(self.map.histogram()[1]) / MEM_SIZE

    def get_avg_holes(self): return float(self.holes) / self.req_counter
    
    def mm_request(self, n, func=0):
        if n > MEM_SIZE: raise BlockTooLarge
        self.req_counter += 1
        ####
        # first-fit
        ####
        if not func: x = self.find_hole(0, MEM_SIZE, n)
        ####
        # next-fit
        ####
        else:
            try: x = self.find_hole(self.next, MEM_SIZE, n)
            except BlockTooLarge: x = self.find_hole(0, self.next, n)
            self.next = x + n
        ####
        # common code
        ####
        return x

    def mm_release(self, p):
        size = self.pcb[p]
        del self.pcb[p]
        for i in range(size):
            self.map.putpixel((p+i, 0), 0)
