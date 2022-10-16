# problema 11 
n = 0
m = 0
exists_output = False

def verifica(new_element, array1):
    if len(array1) == 0:
        return True
    elif abs(array1[-1] - new_element) == m:
        return True
    #elif array1[-1] - new_element == m:
        #return True
    return False

def Back(k, array1):
    if k == n:
        print(array1)
    else:
        for i in range(1,n+1):
            if verifica(i, array1):
                array1.append(i)
                Back(k+1,array1)
                array1.pop()
    
def rezolvare_iterativa(array1):    
    array1 = [0]
    aux = 1
    while len(array1) > 0:
        choosed = False
        while array1[-1] <= n and not choosed:
            array1[-1] += 1
            choosed = verifica(array1[-1], array1[:-1]) and aux != array1[-1]
        aux = array1[-1]
        if choosed:
            if len(array1) == n:
                print(array1)
                array1.pop()
            else:
                array1.append(0)
        else:
            array1.pop()

                
            

if __name__ == '__main__':
    try:
        array1 = []
        n = int(input("n = "))
        m = int(input("m = "))
        #Back(0,array1)
        rezolvare_iterativa(array1)

    except:
        print("Input gresit, n si m trebuie sa fie numere naturale.")

# Cautam vectorii de forma (x1,..,xn) cu oricare xi (i de la 1 la n) din {1,2,..n} in care 
# diferenta absoluta a oricare doua nr consecutive este m 
#
# Spatiul de cautare este A = {1, ..., n}
# Avem n^(n) solutii candidat. 