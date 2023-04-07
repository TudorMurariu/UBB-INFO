'''
    Considerându-se o matrice cu n x m elemente binare (0 sau 1), să se înlocuiască cu 1 toate aparițiile elementelor egale cu 0 care sunt complet înconjurate de 1.

    De ex. matricea \ [
    [1,1,1,1,0,0,1,1,0,1],
    [1,0,0,1,1,0,1,1,1,1],
    [1,0,0,1,1,1,1,1,1,1],
    [1,1,1,1,0,0,1,1,0,1],
    [1,0,0,1,1,0,1,1,0,0],
    [1,1,0,1,1,0,0,1,0,1],
    [1,1,1,0,1,0,1,0,0,1],
    [1,1,1,0,1,1,1,1,1,1]]
    *devine *
    [[1,1,1,1,0,0,1,1,0,1],
    [1,1,1,1,1,0,1,1,1,1],
    [1,1,1,1,1,1,1,1,1,1],
    [1,1,1,1,1,1,1,1,0,1],
    [1,1,1,1,1,1,1,1,0,0],
    [1,1,1,1,1,1,1,1,0,1],
    [1,1,1,0,1,1,1,0,0,1],
    [1,1,1,0,1,1,1,1,1,1]] \
'''

# Time complexity: O(n * m) (lee)
# Space complexity: O(n * m)
def delete_zeros(matrix):
    rez = [[1 for x in range(len(matrix[0]))] for y in range(len(matrix))]

    for i in range(len(matrix)):
        if matrix[i][0] == 0:
            lee(matrix, rez, i, 0)
        if matrix[i][len(matrix[0])-1] == 0:
            lee(matrix, rez, i, len(matrix[0])-1)

    for j in range(len(matrix[0])):
        if matrix[0][j] == 0:
            lee(matrix, rez, 0, j)
        if matrix[len(matrix)-1][j] == 0:
            lee(matrix, rez, len(matrix)-1, j)

    return rez
   
def lee(matrix, rez, x, y):
    if rez[x][y] == 0:
        return None

    rez[x][y] = 0

    if x > 0 and matrix[x-1][y] == 0:
        lee(matrix, rez, x-1, y)
    if x < len(matrix) - 1 and matrix[x+1][y] == 0:
        lee(matrix, rez, x+1, y)  
    if y > 0 and matrix[x][y-1] == 0:
        lee(matrix, rez, x, y-1)
    if y < len(matrix[0]) - 1 and matrix[x][y+1] == 0:
        lee(matrix, rez, x, y+1)         
    
def test():

    assert(delete_zeros([
        [1,1,1,1,0,0,1,1,0,1],
        [1,0,0,1,1,0,1,1,1,1],
        [1,0,0,1,1,1,1,1,1,1],
        [1,1,1,1,0,0,1,1,0,1],
        [1,0,0,1,1,0,1,1,0,0],
        [1,1,0,1,1,0,0,1,0,1],
        [1,1,1,0,1,0,1,0,0,1],
        [1,1,1,0,1,1,1,1,1,1]
        ]) == [
        [1,1,1,1,0,0,1,1,0,1],
        [1,1,1,1,1,0,1,1,1,1],
        [1,1,1,1,1,1,1,1,1,1],
        [1,1,1,1,1,1,1,1,0,1],
        [1,1,1,1,1,1,1,1,0,0],
        [1,1,1,1,1,1,1,1,0,1],
        [1,1,1,0,1,1,1,0,0,1],
        [1,1,1,0,1,1,1,1,1,1]
        ])
    
    assert(delete_zeros([
        [1,1,1,1,1,1,1,1,1,1],
        [1,0,0,1,1,0,1,1,1,1],
        [1,0,0,1,1,1,1,1,1,1],
        [1,1,1,1,0,0,1,1,0,1],
        [1,1,1,1,1,1,1,1,1,1]
        ]) == [
        [1,1,1,1,1,1,1,1,1,1],
        [1,1,1,1,1,1,1,1,1,1],
        [1,1,1,1,1,1,1,1,1,1],
        [1,1,1,1,1,1,1,1,1,1],
        [1,1,1,1,1,1,1,1,1,1]
        ])
    
    assert(delete_zeros([
        [1,1,1,1,1,1,1,1,1,1],
        [1,0,0,1,1,0,1,1,1,1]
        ]) == [
        [1,1,1,1,1,1,1,1,1,1],
        [1,0,0,1,1,0,1,1,1,1],
        ])


if __name__ == '__main__':
    test()