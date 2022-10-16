import Logica


def get_interval(): #Comanda 3     in plus
    interval = input("Alegeti un interval : \n")
    interval = interval.replace(',',' ')
    interval = interval.replace('(',' ')
    interval = interval.replace(')',' ')
    return interval

def afiseaza_interval(a,b,interval): #Comanda 3 , subcomanda 1
    aux = interval.split()
    if len(aux) != 2:
        print("Interval gresit!")
        return
    x,y = aux
    x = int(x)
    y = int(y)
    print(b[x:y+1])
    indici = "  "
    for i in range(x,y+1):
        # daca numarul are mai multe cifre avem nevoie de mai multe spatii
        indici = indici + str(i) + " " * (len(str(b[i])) + 2 - len(str(i))) 
    print(indici)

def modul_mai_mic_10(a,b): #Comanda 3 , subcomanda 2
    ok = True
    s = ""
    for i in range(len(a)):
        if Logica.modulImg(a[i],b[i]) < 10:
            s += a[i] + '+' + b[i] + 'i\n'
            ok = False
    if ok:
        print("Nu exista")
    else:
        print("Toate numerele care au modulul mai mic decat 10 :\n",s)


def modul_egal_10(a,b): #Comanda 3 , subcomanda 3
    ok = True
    s = ""
    for i in range(len(a)):
        if Logica.modulImg(a[i],b[i]) == 10:
            s += a[i] + '+' + b[i] + 'i\n'
            ok = False
    if ok:
        print("Nu exista")
    else:
        print("Toate numerele care au modulul egal cu 10 :\n",s)