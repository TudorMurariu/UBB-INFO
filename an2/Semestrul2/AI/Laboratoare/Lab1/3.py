'''
    Să se determine produsul scalar a doi vectori rari
    care conțin numere reale. Un vector este rar atunci
    când conține multe elemente nule. Vectorii pot avea
    oricâte dimensiuni. De ex. produsul scalar a 2 vectori unisimensionali
    [1,0,2,0,3] și [1,2,0,3,1] este 4.
'''

# Time complexity: O(n^(x)), unde x este numarul de dimensiuni ale vectorului rar
# Space complexity: O(1)
def produs_scalar(a, b):
    if not hasattr(a, "__len__"):
        return a * b

    produs = 0
    for x, y in zip(a, b):
        produs += produs_scalar(x, y)
    return produs

def test():
    assert(produs_scalar([1,0,2,0,3], [1,2,0,3,1]) == 4)
    assert(produs_scalar([[1], [2], [1]], [[0], [3], [0]]) == 6)
    assert(produs_scalar([[[5]], [[5]], [[7]]], [[[0]], [[2]], [[0]]]) == 10)

if __name__ == '__main__':
    test()