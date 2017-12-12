#import hashlib
#md5 = hashlib.md5(b"wtnhxymk")
#i = 0
#pw = []
#pw2 = [-1] * 8
#while 1:
#    hash_ = hashlib.md5(b"wtnhxymk"+bytes(str(i), 'ascii')).hexdigest()
#    if hash_[:5] == '00000':
#        print("got im {} {}", hash_, i)
#        pw.append(hash_[5])
#        key = int(hash_[5], 16)
#        if key < 8 and pw2[key] == -1:
#            pw2[key] = hash_[6]
#            if -1 not in pw2: break
#    i += 1
#    #if i % 1000 == 0:
#    #    fake1 = (''.join(pw[:8]) + hash_[5:13])[:8]
#    #    fake2 = (''.join(map(str,pw2)).replace('-1','') + hash_[13:21])[:8]
#    #    print("{} {}".format(fake1, fake2), end='\r')
#print("{} {}".format(''.join(pw[:8]), ''.join(pw2)), end='\r')

import hashlib
md5 = hashlib.md5(b"wtnhxymk")
i = 0
pw = []
pw2 = [-1] * 8
while 1:
    hash_ = hashlib.md5(b"wtnhxymk"+bytes(str(i), 'ascii')).digest()
    if hash_[0] == 0 and hash_[1] == 0 and hash_[2] & 0xF0 == 0:
        print("got im {} {}", hash_, i)
        pw.append(str(hash_[2]))
        key = int(hex(hash_[3])[2], 16)
        if key < 8 and pw2[key] == -1:
            pw2[key] = str(hex(hash_[3])[2])
            if -1 not in pw2: break
    i += 1
    #if i % 1000 == 0:
    #    fake1 = (''.join(pw[:8]) + hash_[5:13])[:8]
    #    fake2 = (''.join(map(str,pw2)).replace('-1','') + hash_[13:21])[:8]
    #    print("{} {}".format(fake1, fake2), end='\r')
print("{} {}".format(''.join(pw[:8]), ''.join(pw2)), end='\r')
