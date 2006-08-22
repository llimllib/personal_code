import socket

sock = socket.socket()
sock.bind(('localhost', 55000))
sock.listen(1)
while(1):
    (client, addy) = sock.accept()
    b = ' '
    while(len(b)):
        count = 0
        b = client.recv(256)
        for c in b:
            if count % 8 == 0: print
            elif count % 4 == 0: print '  ',
            print '0x%-2x' % ord(c),
            count += 1
        print 'end msg'
