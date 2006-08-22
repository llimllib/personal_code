import asyncore, asynchat, socket, os

class async_server(asyncore.dispatcher):
    """The server class listens on a port, accepts a connection, then passes 
    it off to an async_channel"""
    def __init__(self, port):
        asyncore.dispatcher.__init__(self)
        self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
        self.set_reuse_addr()
        self.bind(('', port))
        self.listen(9999)

    def handle_accept(self):
        print "async_server accepting connection"
        #self.accept() returns a (conn, addr) tuple of a socket and client addy
        async_channel(self, self.accept())

class async_channel(asynchat.async_chat):
    channel_count = 0
    def __init__(self, server, (conn, addr)):
        print "making an async_channel"
        asynchat.async_chat.__init__(self, conn)
        self.set_terminator('\x00')   #strings are null terminated
        self.server = server
        self.id = self.channel_count
        self.channel_count += 1
        self.conn = conn
        self.client_addr = addr
        self.buff = ''

    def collect_incoming_data(self, data):
        self.buff += data

    def found_terminator(self):
        print "found terminator"
        data = self.buff
        self.buff = ''
        print data
        self.push('thanks\n')

    def handle_close(self):
        print 'closing'
        self.channel_count -= 1
        self.close()

if __name__ == "__main__":
    as = async_server(9999)
    asyncore.loop()
