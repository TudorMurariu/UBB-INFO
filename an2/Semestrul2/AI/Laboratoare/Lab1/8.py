'''
    Să se genereze toate numerele (în reprezentare binară) 
    cuprinse între 1 și n. 
    De ex. dacă n = 4, numerele sunt: 1, 10, 11, 100.
'''

# Time complexity: O(n log n)
# Space complexity: O(n * x) , unde x este numarul de biti ale celui mai mare numar din vector
def genereaza_binare(n):
    arr = []
    for i in range(1, n+1):
        arr.append(bin(i)[2:])
    return arr
        
    
def test():
    assert(genereaza_binare(4) == ['1', '10', '11', '100'])
    assert(genereaza_binare(1) == ['1'])
    assert(genereaza_binare(5) == ['1', '10', '11', '100', '101'])


if __name__ == '__main__':
    test()