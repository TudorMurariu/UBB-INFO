def consistent(lst):
    laux = set(lst)
    return len(laux) == len(lst)

def is_solution(a, lst):
    return len(a) == len(lst)

def Back_iterativ(a, lst):
    j = 0
    lst = [-1]
    while len(lst) > 0:
        print(lst)
        choosed = False
        i = j
        while not choosed and i < len(a):
            lst[-1] = a[i]
            choosed = consistent(lst)
            i += 1
        
        if choosed:
            if is_solution(a, lst):
                print(lst, " wwwwwwwww")
                lst = [-1]
                j += 1
            else:
                lst.append(-1)
        else:
            lst.pop()

if __name__ == "__main__":
    txt = input("Scrieti lista : ")
    a = list(map(int,txt.split()))
    lst = []
    Back_iterativ(a, lst)