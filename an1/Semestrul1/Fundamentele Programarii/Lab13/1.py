# def suma_egala(array,S):
#     x = sum(array)
#     if x == S:
#         return True
#     return False

# def verifica_suma(array,S):
#     x = sum(array[:-1])
#     if x + array[-1] > S:
#         return False
#     return True

# def Back_Iter(a, S, array):
#     j = 0
#     array = [-1]
#     while len(array) > 0:
#         print(array)
#         i = j
#         choosed = False
#         while i < len(a) and not choosed:
#             array[-1] = a[i]
#             choosed = verifica_suma(array, S)
#             i += 1
#         j = i
#         if choosed :
#             if suma_egala(array, S):
#                 print(array)
#                 array.pop()
#             else :
#                 array.append(-1)
#         else:
#             array.pop()
#     print("Backtracking terminat")
        


# if __name__ == "__main__":
#     txt = input("Scrieti valorile posibile pentru monede : ")
#     a = list(map(int,txt.split()))
#     a.sort(reverse=True)
#     S = int(input("S = "))

#     print(a)
#     array = []
#     Back_Iter(a, S, array)

def consistent(x):
    """
    The candidate can lead to an actual
    permutation only if there are no duplicate elements
    """
    laux = set(x)
    return len(laux) == len(x)

def solution(x, dim):
    """
    The candidate x is a solution if
    we have all the elements in the permutation
    """
    return len(x) == dim

def backIter(dim):
    x=[-1] #candidate solution
    while len(x)>0:
        print(x)
        choosed = False
        while not choosed and x[-1]<dim-1:
            x[-1] = x[-1]+1 #increase the last component
        choosed = consistent(x)
        if choosed:
            if solution(x, dim):
                print(x)
            x.append(-1) # expand candidate solution
        else:
            x = x[:-1] #go back one component

backIter(4)