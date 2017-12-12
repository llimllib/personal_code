from functools import reduce
from operator import mul, xor


def knot_hash(lengths, rounds=64, sparse_hash=False):
    nums = list(range(256))
    position = 0
    skip_size = 0

    for _ in range(rounds):
        for length in lengths:
            if length > len(nums):
                continue

            # Rotate the list so that we always reverse starting at 0, then
            # rotate it back.
            nums = nums[position:] + nums[:position]
            nums[:length] = reversed(nums[:length])
            nums = nums[-position:] + nums[:-position]

            position += length + skip_size
            position %= len(nums)
            skip_size += 1

    if sparse_hash:
        return nums

    dense_hash = [reduce(xor, nums[16 * i:16 * i + 16], 0) for i in range(16)]

    return ''.join(hex(d)[2:].zfill(2) for d in dense_hash)

with open('input.txt', 'rb') as f:
    lengths = f.read().strip()

sparse_hash = knot_hash(list(map(int, lengths.decode('utf-8').split(','))),
                        rounds=1,
                        sparse_hash=True)
dense_hash = knot_hash(list(lengths) + [17, 31, 73, 47, 23])

print(reduce(mul, sparse_hash[:2], 1))
print(dense_hash)
