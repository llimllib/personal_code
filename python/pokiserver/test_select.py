import socket
import select

def main():
    # set up server socket
    serv_sock = socket.socket()
    serv_sock.setblocking(0)
    #is the sockopt necessary?
    self.socket.setsockopt(socket.SOL_SOCKET,socket.SO_REUSEADDR,1)
    serv_sock.bind(("localhost", 9999))
    serv_sock.listen(4)
    #set up select lists
    readable = []
    writable = []
    while 1:
        client, address = serv_sock.accept()
        readable.append(client)
