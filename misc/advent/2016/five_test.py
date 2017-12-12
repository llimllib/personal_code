import hashlib
md5 = hashlib.md5(b"wtnhxymk")
i = 0
pw = []
pw2 = [-1] * 8
while 1:
    digest = md5.copy()
    digest.update(bytes(str(i), 'ascii'))
    hash_ = digest.hexdigest()
    if hash_.startswith('00000'):
    #hash_ = hashlib.md5(b"wtnhxymk"+bytes(str(i), 'ascii')).digest()
    #if hash_.startswith(b'00000'):
        pw.append(hash_[5])
        key = int(hash_[5], 16)
        if key < 8 and pw2[key] == -1:
            pw2[key] = hash_[6]
            if -1 not in pw2: break
    i += 1
    #if i % 1000 == 0:
    #    fake1 = (''.join(pw[:8]) + hash_[5:13])[:8]
    #    fake2 = (''.join(map(str,pw2)).replace('-1','') + hash_[13:21])[:8]
    #    print("{} {}".format(fake1, fake2), end='\r')
print("{} {}".format(''.join(pw[:8]), ''.join(pw2)), end='\r')
