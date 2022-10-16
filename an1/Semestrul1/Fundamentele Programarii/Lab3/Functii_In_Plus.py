def DifNrPrim7(a,b):
    return isPrime(abs(a-b))

def isPrime(n): #fara Eratostene fiindca nu aveam un nr max mentionat
    if n == 2:
        return True
    elif n % 2 == 0 or n < 2:
        return False
    i = 3
    while i*i <= n:
        if n % i == 0:
            return False
        i += 1
    return True

def SemneDiferite(a,b):
    return a*b < 0

def DouaCfrDistincteComune(a,b):
    cifreA = [0,0,0,0,0,0,0,0,0,0]
    cifreB = [0,0,0,0,0,0,0,0,0,0]
    #         0,1,2,3,4,5,6,7,8,9
    while a:
        cifreA[a%10] = 1
        a = a // 10
    
    while b:
        cifreB[b%10] = 1
        b = b // 10

    nr = 0
    for i in range(10):
        if cifreA[i] and cifreB[i]:
            a += 1
    
    return a >= 2

def AceleasiCifre(a,b):
    cifreA = [0,0,0,0,0,0,0,0,0,0]
    cifreB = [0,0,0,0,0,0,0,0,0,0]
    #         0,1,2,3,4,5,6,7,8,9

    while a:
        cifreA[a%10] = 1
        a = a // 10
    
    while b:
        cifreB[b%10] = 1
        b = b // 10

    for i in range(10):
        if cifreA[i] != cifreB[i]:
            return False
    return True


def MaxLengthSum5(lst,n): #13
    s = [lst[0]]
    for i in range(1,n):
        s.append(s[i-1]+lst[i])
    print(s)
    
    for i in range(n-1,-1,-1):
        for j in range(0,n-i):
            if s[j+i] - s[j] + lst[j] == 5:
                return lst[j:j+i+1]

    return "Nu exista"

def MaxLength_11(lst,n): #11
    s = [lst[0]]
    for i in range(1,n):
        s.append(s[i-1]+lst[i])
    maxS = s[0]-1
    ind1 = -1
    ind2 = -1
    
    for i in range(n-1,-1,-1):
        for j in range(0,n-i):
            if s[j+i] - s[j] + lst[j] > maxS:
                maxS = s[j+i] - s[j] + lst[j]
                ind1 = j
                ind2 = i + j

    return lst[ind1:ind2+1]

def Zero10(a,b):
    return (0 <= a and a <= 10) and (0 <= b and b <= 10)

def VerfProp2(x,command):
    if command == 444:
        return isPrime(x)
    elif command == 8:
        return 0 <= x and x <= 10


def MaxLength_2(lst,n,command):
    l = 0
    lmax = 0
    ind = -1
    for i in range(n):
        if VerfProp2(lst[i],command):
            l += 1
        else:
            l = 0
        
        if l > lmax:
            lmax = l
            ind = i+1
    
    if lmax == -1:
        return "Nu exista"
    return lst[ind - lmax:ind+1]  


def Trei_Valori_Distincte(lst,n): # cerinta  9
    l = 1
    lmax = 1
    ind = 1
    a = lst[0]
    b = lst[1]
    for i in range(2,n):
        if lst[i] == a or lst[i] == b or a == b:
            l += 1
        else:
            l = 1

        a = b
        b = lst[i]

        #print(lst[i - l:i+1],a,b)
        
        if l > lmax:
            lmax = l
            ind = i
    
    return lst[ind - lmax:ind+1] 


def Cerinta2(lst,n): # cerinta  9
    l = 1
    lmax = -1
    ind = 0
    valori = [lst[0]]
    indici = [0]
    for i in range(1,n):
        if lst[i] in valori:
            j = 0
            while valori[j] != lst[i]:
                j += 1
            indici[j] = i
            l += 1
        elif len(valori) < 3:
            valori.append(lst[i])
            indici.append(i)
            l += 1
        else:
            j = 0
            mini = indici[0]
            if mini > indici[1]:
                mini = indici[1]
                j = 1
            if mini > indici[2]:
                mini = indici[2]
                j = 2

            l = i - indici[j]
            del indici[j]
            del valori[j]
            valori.append(lst[i])
            indici.append(i)     

        #print(lst[i-l+1 : i+1])
        
        if l > lmax:
            lmax = l
            ind = i
    
    return lst[ind - lmax+1:ind+1] 

#print(" : : : ",Cerinta2([3,2,3,1,4,5,6,7],8))

def cel_mai_lung_sir_munte(lst,n):
    l = 0
    lmax = -1
    ind = 0
    desc = False
    cresc = False
    for i in range(n-1):
        if lst[i] < lst[i+1] and not desc:
            l += 1
            cresc = True
        elif lst[i] < lst[i+1] and desc:
            l = 1
            desc = False
            cresc = True
        elif lst[i] == lst[i+1]:
            l = 1
            desc = False
            cresc = False
        elif cresc: #daca a fost crescator si acum este descrescator este sir munte
            l += 1
            desc = True
        else:  
            l = 0
        

        #print(l,i,desc,cresc,lst[i - l+1:i+2])
        
        if l > lmax and desc:
            lmax = l
            ind = i
    
    if lmax == -1:
        return "Nu exista un sir munte"
    return lst[ind - lmax+1:ind+2] 


#print(" : : : ",cel_mai_lung_sir_munte([4,3,2,3,1],5))