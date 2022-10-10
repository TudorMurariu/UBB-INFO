l = [1,2,3,2,4,5,2,5]
l2 = [8,9,8]

# a
# substituie(l1 l2 .. ln, x, new_l1..n) = { [] , l = [] }
#                                      { new_l1..n + substituie(l2 l3 .. ln, x, new_l1..n) , l1 == x si l != [] }
#                                      { substituie(l2 l3 .. ln, x, new_l1..n) l1 != x si l != [] }
#

def substituie(l, x, l_new):
    if l == [] :
        return []
    elif l[0] == x:
        return l_new + substituie(l[1:], x, l_new)
    return [l[0]] + substituie(l[1:], x, l_new)
print(substituie(l, 2, l2))



#test a
def test_substituie():
    l = []
    new_l = [98,7,98]
    assert(substituie(l, 3, new_l) == [])
    assert(substituie(l, -453, new_l) == [])
    assert(substituie(l, 0, new_l) == [])
    assert(substituie(l, 13243, new_l) == [])
    l = [1, 2, 2, 4, 5, 2, 7, 2]
    new_l = []
    assert(substituie(l, 2, new_l) == [1,4,5,7])
    new_l = [98,7,98]
    assert(substituie(l, 1, new_l) == [98,7,98, 2, 2, 4, 5, 2, 7, 2])

test_substituie()



l = [1,2,3,2,4,5,2,5]
# b
# poz_n(l2,l3..ln, x) = { null , l == [] }  
#               { l1 , x == 1 si l != [] }
#               { poz_n(l2,l3..ln, x-1) , altfel (l != [] si x != 1) }
#
def poz_n(l, n):
    if l == []:
        return None
    elif n == 1 :
        return l[0]
    return poz_n(l[1:], n-1)

print(poz_n(l, 8))


# test b
def tests_poz():
    l = []
    assert(poz_n(l, 1) == None)
    assert(poz_n(l, 7) == None)
    assert(poz_n(l, 67) == None)

    l = [1, 2, 3]
    assert(poz_n(l, 1) == 1)
    assert(poz_n(l, 2) == 2)
    assert(poz_n(l, 3) == 3)
    assert(poz_n(l, 67) == None)

tests_poz()