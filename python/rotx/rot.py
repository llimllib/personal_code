#!/usr/bin/python

def rotx(x, str):
    str = str.upper()
    res = ''
    for let in str:
        if ord(let) > 64 and ord(let) < 91:
            o = ord(let) + x
            if o > 90: o = 64 + (o - 90)
            res += chr(o)
        else: res += let
    return res

def reverse_rot(str):
    alpha = []
    r = range(26)
    for i in r: alpha.append(chr(90-i))
    str=str.upper()
    res = ''
    for let in str:
        if ord(let)>64 and ord(let) < 91:
            o = ord(let)-65
            res += alpha[o]
        else: res += let
    return res

print reverse_rot("DSZG   RH   GSV   MZNV   LU   JFRMG'H   YLZG   RM   GSV   NLERV   QZDH?")
print reverse_rot("YLB LXGLYVI  FMWVI   Z   YOLLW   IVW   HPB - OREV")
print reverse_rot("XSVVHB   XSZIORV'H   RH   TIVZG.   GSVB   SZEV  Z   TZNV  DSVIV   BLF  KFG RM   Z   WLOOZI   ZMW   BLF   TVG   ULFI   JFZIGVIH.   R   DRM   VEVIB   GRNV!")
