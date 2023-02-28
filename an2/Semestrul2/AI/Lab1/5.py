'''
    Pentru un șir cu n elemente care conține valori 
    din mulțimea {1, 2, ..., n - 1} astfel încât o singură 
    valoare se repetă de două ori, să se identifice acea 
    valoare care se repetă. 
    De ex. în șirul [1,2,3,4,2] valoarea 2 apare de două ori.
'''

# Time complexity: O(n)
# Space complexity: O(1)
def duplicat(array):
    array_sum = sum(array)
    array_length = len(array)
    return array_sum - (array_length * (array_length - 1) // 2)
    
def test():
    assert(duplicat([1,2,3,4,2]) == 2)
    assert(duplicat([1,2,1]) == 1)
    assert(duplicat([1,1]) == 1)


if __name__ == '__main__':
    test()