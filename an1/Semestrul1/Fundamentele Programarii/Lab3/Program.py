# Tema : 3 , 6

#importam funtii
import Functii_In_Plus as Functii

lst = []
n = 0

def citire():
    numberOfElements = int(input("Enter the number of elements:\n"))
    print("Enter the elemnts:")

    '''
    # Citieste cate un element pe fiecare rand
    for i in range(0,n):
        element = int(input())
        lst.append(element)
    ''' 

    # Citeste toate elementele de pe un rand
    elements = input()
    lst1 = elements.split()
    for i in range(numberOfElements):
        lst1[i] = int(lst1[i])
    # sau asa  lst1 = list(map(int,lst1))
    return lst1


def cmmdc(a,b):
    c = 0
    while b :
        c = a % b
        a = b
        b = c
    return a          

def VerificaP(a,b,command):  #nu exista switch in python?? 
    if command == 2:
        return cmmdc(a,b) == 1
    elif command == 111: #cerinta 1
        return a < b
    elif command == 5:
        return a == b
    elif command == 7:
        return Functii.DifNrPrim7(a,b)
    elif command == 12:
        return Functii.SemneDiferite(a,b)
    elif command == 14:
        return Functii.DouaCfrDistincteComune(a,b)
    elif command == 16:  # putea fi facut mai rapid 
        return Functii.AceleasiCifre(a,b)
    return 0 #nu e necesara

def MaxLength(lst,n,command):  #pentru toate problemele cu nr consecutive
    # complexitatea O(n *  ) ~= O(n)
    l = 0
    lmax = 0
    ind = -1
    for i in range(n-1):
        if VerificaP(lst[i],lst[i+1],command):
            l += 1
        else:
            l = 0
        
        if l > lmax:
            lmax = l
            ind = i+1
    
    if lmax == 0:
        return "Nu exista"
    return lst[ind - lmax:ind+1]  

def Disctincte_6(lst,n):   
    l = 0
    lmax = -1
    ind = -1
    auxl = []
    raspuns = []
    for i in range(n):     
        if lst[i] in auxl: 
            while auxl[0] != lst[i]:  # stergem toate elementele pana la gasire lui lst[i]
                del auxl[0]   
                l -= 1
            del auxl[0]  # stergem elementul cu val == lst[i]
            auxl.append(lst[i])  # pentru a il adauga la final
        else:
            l += 1
            auxl.append(lst[i])
        
        if l > lmax :
            lmax = l
            raspuns = auxl.copy() # copiem in raspuns elementele listei auxl 
    
    return raspuns

def Cerinta_10(lst,n):  
    l = 2
    lmax = -1
    ind = 0
    for i in range(n-2):
        if Functii.SemneDiferite(lst[i+1] - lst[i],lst[i+2] - lst[i+1]):
            l += 1
        else:
            l = 2
        
        if l > lmax:
            lmax = l
            ind = i+3
    
    if lmax < 3:
        return [lst[0]]
    return lst[ind - lmax:ind]

print(Cerinta_10([10,3,4,2,1,-1],6))

# Main
while True:
    command = input("Scrieti o comanda.\n")
    if command == "print":
        print(n,lst)
    else:
        command = int(command)

        # comenzi tema : 3,6

        if command == 1: 
            #Citire lista
            lst = citire()
            n = len(lst)
        elif command == 2:  
            #Cel mai mare subsir in care elementele consecutive au cmmdc 1   -  3
            print(MaxLength(lst,n,command))
        elif command == 3:  
            #Cel mai mare subsir cu elemente distincte                       -  6 
            print(Disctincte_6(lst,n))
        elif command == 4: 
            #Iesire
            print("Bye bye!")
            break


        elif command == 10:
            print(Cerinta_10(lst,n))


        # comenzi in plus   
        elif command == 111: 
            #cerinta 1
            print(MaxLength(lst,n,command))
        elif command == 222:
            #cerinta 2
            print(Functii.Cerinta2(lst,n))
        elif command == 444:
            #cerinta 4
            print(Functii.MaxLength_2(lst,n,command))
        elif command == 5:
            #toate elementele egale
            print(MaxLength(lst,n,command))
        elif command == 7:
            #Cel mai mare subsir in care elementele consecutive au dif un nr prim
            print(MaxLength(lst,n,command))
        elif command == 8:
            print(Functii.MaxLength_2(lst,n,command))
        elif command == 9:
            #cerinta 2
            print(Functii.Trei_Valori_Distincte(lst,n))
        elif command == 11: 
            #suma maxima
            print(Functii.MaxLength_11(lst,n))    
        elif command == 12: 
            #elemente consecutive cu semn diferit
            print(MaxLength(lst,n,command))
        elif command == 13: 
            #suma elementelor este egal cu 5
            print(Functii.MaxLengthSum5(lst,n))
        elif command == 14: 
            #cel putin 2 cifre distincte comune
            print(MaxLength(lst,n,command))
        elif command == 15:
            #cel mai lung sir munte
            print(Functii.cel_mai_lung_sir_munte(lst,n))
        elif command == 16:
            #scrierea lor in baza 10 foloseste aceleasi cifre
            print(MaxLength(lst,n,command))

