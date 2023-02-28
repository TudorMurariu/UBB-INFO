'''
    Pentru un șir cu n numere întregi care conține și duplicate
    , să se determine elementul majoritar (care apare de mai mult de n / 2 ori).
    De ex. 2 este elementul majoritar în șirul [2,8,7,2,2,5,2,3,1,2,2].
'''

# Time complexity: O(n)
# Space complexity: O(1)
def element_majoritar(array):
    maxi = "default value"
    contor = 0
    for x in array:
        if contor == 0:
            maxi = x
            contor = 1
        elif x == maxi:
            contor += 1
        else: # x != maxi
            contor -= 1
    return maxi
    
def test():
    assert(element_majoritar([2,8,7,2,2,5,2,3,1,2,2]) == 2)
    assert(element_majoritar([1,2,1,9,9,3,1]) == 1)
    assert(element_majoritar([1,1,5,4,3,5,5,7]) == 5)


if __name__ == '__main__':
    test()