'''
    Să se genereze toate numerele (în reprezentare binară) 
    cuprinse între 1 și n. 
    De ex. dacă n = 4, numerele sunt: 1, 10, 11, 100.
'''

# Time complexity: O(n)
# Space complexity: O(1)
def genereaza_binare(n):
    arr = []
    actual = "1"
    for i in range(1, n):
        arr.append(actual)
        
    
def test():
    assert(genereaza_binare([2,8,7,2,2,5,2,3,1,2,2]) == 2)
    assert(genereaza_binare([1,2,1,9,9,3,1]) == 1)
    assert(genereaza_binare([1,1,5,4,3,5,5,7]) == 5)


if __name__ == '__main__':
    test()