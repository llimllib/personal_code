import sys

from cycpu import cpu

if __name__ == "__main__":
    input_file = sys.argv[1]
    input_buf = sys.argv[2:]

    mem = [int(n) for n in open(input_file).read().strip().split(",")]

    cpu(mem, input_buf)
