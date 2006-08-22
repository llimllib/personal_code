#!/usr/bin/python
import os, sys, asyncore, asynchat, socket

DEBUG = sys.stderr

class pitch_server(asyncore.dispatcher):
    def __init__(self):
        asyncore.dispatcher.__init__(self)
        self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
        self.set_reuse_addr()
        self.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.bind(('localhost',812))
        self.listen(1)
        print >> DEBUG, "Server started on port 812"
        asyncore.loop()
    
    def handle_accept(self):
        conn, addr = self.accept()
        print >> DEBUG, "Aceepting connection on %s" % repr(addr)
        channel = pitch_channel(self, conn, addr)

class pitch_channel(asynchat.async_chat):
    def __init__(self, server, conn, addr):
        asynchat.async_chat.__init__(self, conn)
        self.set_terminator('\n')
        self.__server = server
        self.__conn = conn
        self.__addr = addr
        self.__line = []
        self.__greeting = 0
        self.__rcpttos = []
        self.__data = ''
        self.__fqdn = socket.getfqdn()
        self.__peer = conn.getpeername()
        print >> DEBUG, 'Peer:', repr(self.__peer)
        self.push('name')

    # Overrides base class for convenience
    def push(self, msg):
        asynchat.async_chat.push(self, msg + '\n')

    # Implementation of base class abstract method
    def collect_incoming_data(self, data):
        self.__line.append(data)
        print >> DEBUG, 'Inc_Data: %s' % repr(data)

    # Implementation of base class abstract method
    def found_terminator(self):
        line = ''.join(self.__line)
        print >> DEBUG, 'Data:', repr(line)
        self.__line = []
        line = line[:line.find('#')]
        line = line.replace('\t', ' ').strip()
        if line[:1] != '=':
            self.respond_error(line)
        else:
            arg = line[2:]
        method = getattr(self, 'pitch_' + command, None)
        if not method:
            self.push('? error' % command)
            return
        method(arg)

if __name__ == '__main__':
    p = pitch_server()
