import string

print(len(string.ascii_lowercase))


def nr_neg(l):
    if len(l) == 1:
        return l[0] < 0
    mij = (len(l))//2
    return nr_neg(l[0:mij]) + nr_neg(l[mij:])

print(nr_neg([-1,-10,0,8,-9,-9,-9]))
