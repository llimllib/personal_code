FREE = 0
BUSY = 1

class process:
    def __init__(self):
        """process control block"""
        self.id = 0                   #process id number
        self.memory = None            #not yet implemented
        self.other_resources = []     #resources requested by the process
        self.status = {'type':None, 'list':None}
        self.creation_tree = {'parent':None, 'child':[]}
        self.priority = 0             #integer priority for the process
        self.name = ''                #process name

class resource:
    def __init__(self, id):
        """resources are virtual kernel resources: printers, HDs, monitors, etc.

        They each have an ID number, a status, and an access queue. They are 
        created at startup time."""
        self.id = id                  #resource id number
        self.status = FREE            #device is currently unused
        self.waiting_list = []        #list of processes blocked on resource

class process_queue:
    def __init__(self):
        self._queue = []

    def print_cr_tree(self, tree):
        """help pretty pring a list of processes and their children"""
        str = ''
        try:
            if not tree: return "None"
            else:
                for x in tree: str += " " + x.name
        except TypeError: return tree.name
        return str

    def process_list(self):
        """print a list of processes and information about them"""
        for p in self._queue:
            print "%-5d %-10s %-10s %2d %10s %10s" % (p.id, p.name,
                p.status['type'], p.priority, 
                self.print_cr_tree(p.creation_tree['parent']), 
                self.print_cr_tree(p.creation_tree['child']))

    def push(self, elt):
        """pushes elt onto the process queue

        elt must be a process with a priority"""
        if len(self._queue) == 0: self._queue.append(elt); return
        for i in range(len(self._queue)):
            if self._queue[i].priority < elt.priority:
                self._queue.insert(i, elt)
                return
        #if we get here, elt is lower than all the other procs in the queue, so
        #just append it
        self._queue.append(elt)

    def pop(self, pid):
        """pop an element with pid from the queue"""
        for p in self._queue:
            if p.id == pid:
                return self._queue.pop(self._queue.index(p)).id
        return 0

    def top(self):
        """return the highest priority process from the queue"""
        try: return self._queue[0]
        except IndexError: return 0

    def get_pid(self, pid):
        """return a pcb with a given a pid"""
        for p in self._queue:
            if p.id == pid:
                return p
        else: return 0
