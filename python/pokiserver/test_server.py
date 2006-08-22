from socket import socket

def test_connect():
    sock = socket()
    sock.connect(('localhost', 9999))
    data = "How you doing?\x0011111111"
    msg = ltos(len(data)) + data
    to_send = len(msg)
    while to_send:
        to_send -= sock.send(msg)
    raw_input()

def ltos(n):
    """convert a long into a string"""
    s = ''
    for x in (2**24, 2**16, 2**8):
        quot, rem = (n/x, n%x)
        s += chr(quot)
        n = rem
    s += chr(n % 256)
    return s

test_connect()
