'''
    Considerându-se o matrice cu n x m elemente binare (0 sau 1)
    sortate crescător pe linii, să se identifice indexul liniei
    care conține cele mai multe elemente de 1.
    De ex. în matricea
        [[0,0,0,1,1],
        [0,1,1,1,1],
        [0,0,1,1,1]]
    a doua linie conține cele mai multe elemente 1.
'''


# Time complexity: O(n * log(n))
# Space complexity: O(1)
def index_max1_line(matrix):
    st = 0
    dr = len(matrix) - 1

    while st <= dr:
        mid = (st + dr) // 2
        contor = 0
        index = -1
        for i in range(0, len(matrix)):
            if matrix[i][mid] == 1:
                contor += 1
                index = i
        
        if contor == 1:
            return index
        elif contor == 0:
            st = mid + 1
        else: # contor > 1
            dr = mid - 1

    return 1
   
        
    
def test():
    assert(index_max1_line([
        [0,0,0,1,1],
        [0,1,1,1,1],
        [0,0,1,1,1]
        ]) == 1)
    assert(index_max1_line([
        [0,0,0,1,1],
        [1,1,1,1,1],
        [0,0,0,0,1],
        [0,0,0,0,0],
        [0,0,1,1,1]
        ]) == 1)
    assert(index_max1_line([
        [0,0,0,0,1],
        [0,0,0,0,0],
        [0,0,0,0,0],
        [0,0,0,0,0],
        [0,0,0,1,1],
        [0,0,0,0,0]
        ]) == 4)


if __name__ == '__main__':
    test()