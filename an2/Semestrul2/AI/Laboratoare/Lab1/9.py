'''
    Considerându-se o matrice cu n x m elemente întregi
      și o listă cu perechi formate din coordonatele a 2 
      căsuțe din matrice ((p,q) și (r,s)), să se calculeze 
      suma elementelor din sub-matricile identificate de fieare pereche.
'''


# Time complexity: O(n * m)
# Space complexity: O(n * m)
def sums_matrix(matrix, list):
    sume_partiale = [[0 for x in range(len(matrix[0]) + 1)] for y in range(len(matrix) + 1)]
    for i in range(1, len(matrix) + 1):
        for j in range(1, len(matrix[0]) + 1):
            sume_partiale[i][j] = sume_partiale[i][j-1] + sume_partiale[i-1][j] -  sume_partiale[i-1][j-1] + matrix[i-1][j-1]

    rez = []
    for x in list:
        a = x[0][0]
        b = x[0][1] 
        c = x[1][0] + 1
        d = x[1][1] + 1
        new_elem = sume_partiale[c][d] - sume_partiale[c][b] - sume_partiale[a][d] + sume_partiale[a][b]
        rez.append(new_elem)

    return rez
   
        
    
def test():
    assert(sums_matrix([
        [0, 2, 5, 4, 1],
        [4, 8, 2, 3, 7],
        [6, 3, 4, 6, 2],
        [7, 3, 1, 8, 3],
        [1, 5, 7, 9, 4]], 
        [ [ [1, 1], [3, 3] ], [ [2, 2], [4, 4] ] ]) == [38, 44])
    assert(sums_matrix([
        [0, 2, 3],
        [4, 8, 2],
        [6, 3, 4]],
        [ [ [1, 1], [1 ,1] ], [ [0, 0], [1, 1] ], [ [0,0], [0,0] ] ]) == [8, 14, 0])
    assert(sums_matrix([
        [0, 2, 3],
        [4, 8, 2],
        [6, 3, 4]],
        []) == []) # test cu input lista vida


if __name__ == '__main__':
    test()