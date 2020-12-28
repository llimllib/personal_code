def trans(subj, loops):
    n = 1
    for i in range(0, loops):
        n *= subj
        n %= 20201227
    return n


def t2(target):
    n = 1
    i = 1
    while 1:
        n *= 7
        n %= 20201227
        if n == target:
            return i
        i += 1
    return i


def enc_key(card_pub_key, door_pub_key):
    card_priv_key = t2(card_pub_key)
    door_priv_key = t2(door_pub_key)
    assert trans(card_pub_key, door_priv_key) == trans(door_pub_key, card_priv_key)
    return trans(card_pub_key, door_priv_key)


# example
assert t2(5764801) == 8
assert t2(17807724) == 11
assert trans(17807724, 8) == 14897079
assert trans(5764801, 11) == 14897079
assert enc_key(5764801, 17807724) == 14897079
assert enc_key(17807724, 5764801) == 14897079

# my input
card_pub_key = 12090988
door_pub_key = 240583

print(f"part 1: {enc_key(door_pub_key, card_pub_key)}")
