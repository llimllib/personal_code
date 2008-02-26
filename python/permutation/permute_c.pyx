cdef class Permute2:

    cdef int n, first
    cdef object lst

    def __init__(self, lst):
        self.lst = lst
        self.first = 0
        self.n = len(lst) - 1

    def __iter__(self):
        return self

    def __next__(self):
        cdef int j, l, k, x, y, z
        if self.first == 0:
            self.first = 1
            return self.lst

        if self.n == 0: raise StopIteration
        if self.n == 1:
            if self.lst[0] < self.lst[1]:
                self.lst[0], self.lst[1] = self.lst[1], self.lst[0]
                return self.lst
            raise StopIteration

        while 1:
            if self.lst[-2] < self.lst[-1]:
                self.lst[-2], self.lst[-1] = self.lst[-1], self.lst[-2]
            elif self.lst[-3] < self.lst[-2]:
                if self.lst[-3] < self.lst[-1]:
                    self.lst[-3], self.lst[-2], self.lst[-1] = self.lst[-1], self.lst[-3], self.lst[-2]
                else:
                    self.lst[-3], self.lst[-2], self.lst[-1] = self.lst[-2], self.lst[-1], self.lst[-3]
            else:
                j = self.n - 3
                if j < 0: raise StopIteration
                y = self.lst[j]
                x = self.lst[-3]
                z = self.lst[-1]
                while y >= x:
                    j = j - 1
                    if j < 0: raise StopIteration
                    x = y
                    y = self.lst[j]
                if y < z:
                    self.lst[j] = z
                    self.lst[j+1] = y
                    self.lst[self.n] = x
                else:
                    l = self.n - 1
                    while y >= self.lst[l]:
                        l = l - 1
                    self.lst[j], self.lst[l] = self.lst[l], y
                    self.lst[self.n], self.lst[j+1] = self.lst[j+1], self.lst[self.n]
                k = j + 2
                l = self.n - 1
                while k < l:
                    self.lst[k], self.lst[l] = self.lst[l], self.lst[k]
                    k = k + 1
                    l = l - 1
            return self.lst
