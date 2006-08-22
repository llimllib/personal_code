class proxy_server (asyncore.dispatcher):
    
    def __init__ (self, host, port):
        asyncore.dispatcher.__init__ (self)
        self.create_socket (socket.AF_INET, socket.SOCK_STREAM)
        self.set_reuse_addr()
        self.there = (host, port)
        here = ('', port + 8000)
        self.bind (here)
        self.listen (5)

    def handle_accept (self):
        proxy_receiver (self, self.accept())

class proxy_receiver (asynchat.async_chat):

    channel_counter = 0

    def __init__ (self, server, (conn, addr)):
        asynchat.async_chat.__init__ (self, conn)
        self.set_terminator ('\n')
        self.server = server
        self.id = self.channel_counter
        self.channel_counter = self.channel_counter + 1
        self.sender = proxy_sender (self, server.there)
        self.sender.id = self.id
        self.buffer = ''

    def collect_incoming_data (self, data):
        self.buffer = self.buffer + data
        
    def found_terminator (self):
        data = self.buffer
        self.buffer = ''
        print '<== (%d) %s' % (self.id, repr(data))
        self.sender.push (data + '\n')

     def handle_close (self):
         print 'Closing'
         self.sender.close()
         self.close()      

class proxy_sender (asynchat.async_chat):

    def __init__ (self, receiver, address):
        asynchat.async_chat.__init__ (self)
        self.receiver = receiver
        self.set_terminator (None)
        self.create_socket (socket.AF_INET, socket.SOCK_STREAM)
        self.buffer = ''
        self.set_terminator ('\n')
        self.connect (address)

    def handle_connect (self):
        print 'Connected'

    def collect_incoming_data (self, data):
        self.buffer = self.buffer + data

    def found_terminator (self):
        data = self.buffer
        self.buffer = ''
        print '==> (%d) %s' % (self.id, repr(data))
        self.receiver.push (data + '\n')

     def handle_close (self):
         self.receiver.close()
         self.close()

if __name__ == '__main__':
    import sys
    import string
    if len(sys.argv) < 3:
        print 'Usage: %s <server-host> <server-port>' % sys.argv[0]
    else:
        ps = proxy_server (sys.argv[1], string.atoi (sys.argv[2]))
        asyncore.loop()  
