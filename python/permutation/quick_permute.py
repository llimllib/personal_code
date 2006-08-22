def partial_reverse(lst, start, end):
    end -= 1
    while start <= end:
        lst[start], lst[end] = lst[end], lst[start]
        start += 1
        end -= 1
    return lst

def next_perm(lst):
    first, last = 0, len(lst)
    if last in (0, 1): return
    i = last - 1
    while 1:
        ii = i
        i -= 1
        if lst[i] < lst[ii]:
            j = last - 1
            while lst[i] >= lst[j]:
                j -= 1
            lst[i], lst[j] = lst[j], lst[i]
            partial_reverse(lst, ii, last)
            return
        if i == first:
            return
