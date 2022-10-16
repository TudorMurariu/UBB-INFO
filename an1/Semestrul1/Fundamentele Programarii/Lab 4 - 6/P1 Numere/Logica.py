import math

def modulImg(a,b): #module of an complex number
    return math.sqrt(a*a + b*b)

def make_a_copy(first_list): #makes a copy of a 1d list 
    new_list = []                                   
    for el in first_list:
        new_list.append(el) 
    return new_list

def Eratostene(n): # for prime numbers
    Prime = {0:0 , 1:0}
    for i in range(2,n+1):
        if not(i in Prime.keys()):
            Prime[i] = 1
            j = i*i
            while j < n*n:  
                Prime[j] = 0
                j += i
    return Prime

def Suma_Nr_Complexe(a,b): #Comanda 4 , subcomanda 1
    x = sum(a)
    y = sum(b)
    if y < 0 :
        c = '' 
    else:
        c = '+'
    return str(x) + c + str(y) + 'i'

def test_Suma_Nr_Complexe():
    assert Suma_Nr_Complexe([1,2,3],[1,2,3]) == "6+6i"
    assert Suma_Nr_Complexe([2],[2]) == "2+2i"
    assert Suma_Nr_Complexe([-1,-2,4],[1,-2,3]) == "1+2i"
    assert Suma_Nr_Complexe([-1,-5],[1,-2]) == "-6-1i"
    assert Suma_Nr_Complexe([-1,7,2,-2],[1,-2,-9,1]) == "6-9i"

def Produs_Nr_Complexe(a,b): #Comanda 4 , subcomanda 2
    nReal = 1
    nImg = 0
    # (nReal + nImg*I) * (i + j*I) = nReal*i - nImg*j + (nImg*i + nReal*j)*I

    for i,j in zip(a,b):
        auxReal = nReal * i - nImg * j
        nImg = nReal * j + nImg * i  
        nReal = auxReal

    if nImg < 0 :
        c = '' 
    else:
        c = '+'
    
    return str(nReal) + c + str(nImg) + 'i'

def test_Produs_Nr_Complexe():
    assert Produs_Nr_Complexe([1,2],[2,1]) == "0+5i"
    assert Produs_Nr_Complexe([3],[-1]) == "3-1i"
    assert Produs_Nr_Complexe([8,-3,9],[-1,7,8]) == "-625+395i"
    assert Produs_Nr_Complexe([7,9],[3,-5]) == "78-8i"

def Sortare(a,b): #Comanda 4 , subcomanda 3
    #B sort descrescator
    ok = True
    while ok:
        ok = False
        for i in range(0,len(a)-1):
            if b[i] < b[i+1]:
                ok = True
                a[i],a[i+1] = a[i+1],a[i]
                b[i],b[i+1] = b[i+1],b[i]

def test_Sortare():
    a1 = [1,2,3,4]
    b1 = [7,3,5,4]
    Sortare(a1,b1)
    assert a1 == [1,3,4,2] and b1 == [7,5,4,3]
    a2 = []
    b2 = []
    Sortare(a2,b2)
    assert a2 == [] and b2 == []
    a3 = [-1,-3,5]
    b3 = [1,2,3]
    Sortare(a3,b3)
    assert a3 == [5,-3,-1] and b3 == [3,2,1]
    a4 = [1,2,3,4,5]
    b4 = [5,4,3,2,1]
    Sortare(a4,b4)
    assert a4 == [1,2,3,4,5] and b4 == [5,4,3,2,1]

def strege_partea_reala_nr_prim(a,b): #Comanda 5 , subcomanda 1
    try:
        Prime = Eratostene(max(a)) #Eratostene pentru cel mai mare numar din lista a
        i = 0 
        while i < len(a): 
            if Prime[a[i]] == 1:
                del a[i]
                del b[i]
            else:
                i += 1
    except:
        return    # in cazul in care lista este vida max(a) va aduce o eroare
                  # iar lista raspuns trebuie sa fie tot o lista vida

def test_strege_partea_reala_nr_prim():
    a1 = [1,2,4,6,7]
    b1 = [1,2,3,4,5]
    strege_partea_reala_nr_prim(a1,b1)
    assert a1 == [1,4,6] and b1 == [1,3,4]
    a2 = [5,5,5,5]
    b2 = [1,2,3,4]
    strege_partea_reala_nr_prim(a2,b2)
    assert a2 == [] and b2 == []
    a3 = []
    b3 = []
    strege_partea_reala_nr_prim(a3,b3)
    assert a3 == [] and b3 == []
    a4 = [4,6,8,9]
    b4 = [3,4,5,6]
    strege_partea_reala_nr_prim(a4,b4)
    assert a4 == [4,6,8,9] and b4 == [3,4,5,6]

def eliminare_modul(a,b,semn,numar): #Comanda 5 , subcomanda 2
    i = 0
    while i < len(a):
        if Verf(modulImg(a[i],b[i]),numar,semn):
            del a[i]
            del b[i]
        else:
            i += 1

def test_eliminare_modul():
    a1 = [10,7,14,0]
    b1 = [0,13,5,10]
    numar1 = 10  
    semn1 = '='
    eliminare_modul(a1,b1,semn1,numar1)
    assert a1 == [7,14] and b1 == [13,5]
    a2 = [0,1,5,8]
    b2 = [4,2,5,9]
    numar2 = 4
    semn2 = '<'
    eliminare_modul(a2,b2,semn2,numar2)
    assert a2 == [0,5,8] and b2 == [4,5,9]
    a3 = [0,7,19,55,1]
    b3 = [8,7,13,12,2]
    numar3 = 8
    semn3 = '>'
    eliminare_modul(a3,b3,semn3,numar3)
    assert a3 == [0,1] and b3 == [8,2]

def Verf(x,y,semn):
    if semn == '=':
        return x == y
    elif semn == '<':
        return x < y
    else:
        return x > y

#Teste
test_Sortare()
test_Suma_Nr_Complexe()
test_Produs_Nr_Complexe()
test_strege_partea_reala_nr_prim()
test_eliminare_modul()