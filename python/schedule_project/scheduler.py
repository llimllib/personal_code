from process import *
import time

class Scheduler:
    def __init__(self):
        self.max_pids = 1000
        self.cur_pid = 0
        self.proc_list = {}     #references to all our processes
        self.cur_proc = 0 
        self.ready_a_q = process_queue()
        self.ready_s_q = process_queue()
        self.blck_a_q = process_queue()
        self.blck_s_q = process_queue()
        self.funcs = {'req':self.request,\
                      'rel':self.release,\
                      'cr':self.create,\
                      'kill':self.destroy,\
                      'susp':self.suspend,\
                      'act':self.activate,\
                      'ps':self.ps,\
                      'time':self.timeout,\
                      'req_io':self.request_io,\
                      'rel_io':self.release_io}
        self.res = [resource(rid) for rid in range(4)] #our 4 resources
        self.io = resource(99)
        self.create(['Init', 0])
        global FREE, BUSY       #get values from global scope

    def ps(self, args):
        """disply info about all running processes and resources"""
        p = self.cur_proc
        print "ID    Name       Status     Pri    Parent   Children"
        print "%-5d %-10s %-10s %2d %10s %10s" % (p.id, p.name,
            p.status['type'], p.priority, 
            self.ready_a_q.print_cr_tree(p.creation_tree['parent']), 
            self.ready_a_q.print_cr_tree(p.creation_tree['child']))
        for q in (self.ready_a_q, self.ready_s_q, self.blck_s_q, self.blck_a_q):
            q.process_list()
        print "\nRID  Status   Waiting List"
        for r in self.res: 
            print "%-4d %-8d %-20s" % (r.id, r.status,
                self.p_list(r.waiting_list))

    def p_list(self, list):
        """return a string to pretty print out a list of processes"""
        str = ''
        for p in list: str += p.name + ' '
        return str

    def ex(self, func, args):
        """execute the given function with arguments args"""
        self.funcs[func](args)

    def has_func(self, func):
        """returns true if the scheduler has a function called func"""
        return self.funcs.has_key(func)

    def timeout(self, args):
        """simulates an IO timeout"""
        self.cur_proc.status['type'] = 'ready_a'
        eval('self.%s_q.push(%s)' % (self.cur_proc.status['type'],\
            "self.cur_proc"))
        self.cur_proc = 0
        self.schedule()

    def request_io(self, args):
        """simulate an IO request"""
        self.cur_proc.status['type'] = 'blck_a'
        self.cur_proc.status['list'] = self.io
        self.io.waiting_list.append(self.cur_proc)
        eval('self.%s_q.push(%s)' % (self.cur_proc.status['type'],\
            "self.cur_proc"))
        self.cur_proc = 0
        self.schedule()
        
    def release_io(self, args):
        """simulate an IO release (called completion in the book)"""
        p = self.io.waiting_list.pop()
        eval('self.%s_q.pop(%d)' % (p.status['type'], p.id))
        p.status['type'] = 'ready_a'
        p.status['list'] = None
        eval('self.%s_q.push(%s)' % (p.status['type'], "p"))
        self.schedule()
 
    def request(self, args):
        """request a resource"""
        r = self.get_rcb(int(args[0]))
        if r.status == FREE:
            r.status = BUSY
            self.cur_proc.other_resources.append(r)
        else:
            self.cur_proc.status['type'] = 'blck_a'
            self.cur_proc.status['list'] = r
            eval('self.%s_q.push(%s)' % (self.cur_proc.status['type'],\
                "self.cur_proc"))
            r.waiting_list.append(self.cur_proc)
            self.cur_proc = 0
        self.schedule()
    
    def release(self, args):
        """release a resource"""
        r = self.get_rcb(int(args[0]))
        if r.waiting_list == []:
            r.status = FREE
            p = self.cur_proc
            if p: p.other_resources.pop(p.other_resources.index(r))
        else:
            p = r.waiting_list.pop()
            eval('self.%s_q.pop(%d)' % (p.status['type'], p.id))
            if p.status['type'] == 'blck_s': p.status['type'] = 'ready_s'
            else: p.status['type'] = 'ready_a'
            eval('self.%s_q.push(%s)' % (p.status['type'], "p"))
        self.schedule()

    def get_rcb(self, rid):
        """find an rcb given an rid"""
        for rcb in self.res:
            if rcb.id == rid: return rcb

    def find_pcb(self, pid):
        """try to find the pcb referred to by pid.

        pid may be an integer or a string equal to the process' name"""
        p = None
        try: p = self.proc_list[int(pid)]
        except KeyError:
            # p wasn't an ID in proc_list
            print "no process found with id %d\n" % pid
            raise ValueError
        except ValueError:
            # see if p is a name in proc_list
            for proc in self.proc_list.itervalues():
                if proc.name == pid: p = proc
            if p == None:
                print "no process found with name %s\n" % pid
                return None
        return p

    def kill_tree(self, proc):
        """kill a process and all its children recursively"""
        for p in proc.creation_tree['child']: self.kill_tree(p)
        del self.proc_list[proc.id]
        if proc.status['type'] == 'running': self.cur_proc = 0
        else:
            eval('self.'+proc.status['type']+'_q.pop('+str(proc.id)+')')
        for x in proc.other_resources:
            self.release([x.id])
 
    def destroy(self, args):
        """destroy a process"""
        p = self.find_pcb(args[0])
        if not p: return
        if p.name == "Init": return
        if p.id == 1: print "cannot destroy the Init process"; return
        self.kill_tree(p)
        #remove p's parent's copy of p
        p.creation_tree['parent'].creation_tree['child'].remove(p)
        print "process %s destroyed\n" % args[0]
        self.schedule()

    def get_pid(self):
        """generate a new, unused pid"""
        pid = (self.cur_pid + 1) % self.max_pids
        #assumes that all pids are not filled
        while self.proc_list.has_key(pid) or not pid: 
            pid = (pid + 1) % self.max_pids
        return pid

    def create(self, args):
        """create a new process"""
        args[1] = int(args[1])
        proc = process()
        self.cur_pid = self.get_pid()
        proc.id = self.cur_pid
        proc.status['type'] = 'ready_a'
        proc.creation_tree['parent'] = self.cur_proc
        proc.creation_tree['child'] = []
        proc.name = args[0]
        proc.priority = args[1]
        if self.cur_proc:
            self.cur_proc.creation_tree['child'].append(proc)
        else: proc.creation_tree['parent'] = None
        self.proc_list[proc.id] = proc
        self.ready_a_q.push(proc)
        print "process %s created with priority %d\n" % \
              (proc.name, proc.priority)
        self.schedule()
    
    def suspend(self, args):
        """suspend a process"""
        proc = self.find_pcb(args[0])
        if proc.status['type'] != 'running':
            #pop the process from its stack if it's not running
            eval('self.%s_q.pop(%d)' % (proc.status['type'], proc.id))
        if proc.status['type'] == 'blck_a' or proc.status['type'] == 'blck_s':
            proc.status['type'] = 'blck_s'
        else: proc.status['type'] = 'ready_s'
        #push the process onto its new stack
        eval('self.%s_q.push(%s)' % (proc.status['type'], "proc"))
        print "Process %d suspended\n" % proc.id
        self.schedule()
    
    def activate(self, args):
        """activate a process"""
        proc = self.find_pcb(args[0])
        if proc.status['type'] == 'running' or proc.status['type'] == \
            'ready_a' or proc.status['type'] == 'blck_a':
            print "Process already activated"; return
        eval('self.%s_q.pop(%d)' % (proc.status['type'], proc.id))
        if proc.status['type'] == 'blck_s': proc.status['type'] = 'blck_a'
        else: proc.status['type'] = 'ready_a'
        eval('self.%s_q.push(%s)' % (proc.status['type'], "proc"))
        print "Process %d activated\n" % proc.id
        self.schedule()
    
    def schedule(self):
        """schedule a process to run"""
        p = self.ready_a_q.top()
        if not p: return
        if not self.cur_proc or self.cur_proc.priority < p.priority\
            or self.cur_proc.status['type'] != 'running':
            self.ready_a_q.pop(p.id)
            p.status['type'] = 'running'
            if self.cur_proc and self.cur_proc.status['type'] == 'running':
                self.cur_proc.status['type'] = 'ready_a'
                self.ready_a_q.push(self.cur_proc)
            self.cur_proc = p
        print "Process %s is now running" % self.cur_proc.name
