import UI
import Logica 

def Citire(a,b,listStates,txt,indiceLista):  #Comanda 1
    # pentru cazul in care numarul e de forma x-yi  
    aux = txt[:]
    aux = aux.replace('1','')
    aux = aux.replace('2','')
    aux = aux.replace('3','')
    aux = aux.replace('4','')
    aux = aux.replace('5','')
    aux = aux.replace('6','')
    aux = aux.replace('7','')
    aux = aux.replace('8','')
    aux = aux.replace('9','')
    aux = aux.replace('0','')
    aux = aux.replace(' ','')
    aux = aux.replace('i','')

    minusImg = False
    minusReal = False
    if aux.count('-') == 2:
        minusImg = True
        minusReal = True
    elif aux.count('-') == 1 and txt.find('+') < 0:
        minusImg = True
    elif aux.count('-') == 1:
        minusReal = True
        
    # transformam textul in numerele de care avem nevoie
    txt = txt.replace('i',' ')
    txt = txt.replace('+',' ')
    txt = txt.replace(',',' ')
    txt = txt.replace('-',' ')
    auxl = txt.split() 
    
    try:
        x = int(auxl[0])
        y = int(auxl[1])
        if minusImg:
            y = y * -1
        if minusReal:
            x = x * -1

        if len(auxl) > 2:  # daca avem si o pozitie mentionata
            poz = int(auxl[2])
            if poz > len(a)+1:
                print("Pozitia introdusa nu exista.")
            else:
                #adaugam in sir numarul la pozitia mentionata
                a.append(1)
                b.append(1)
                
                for i in range(len(a)-1,poz,-1):
                    a[i] = a[i-1]
                    b[i] = b[i-1]
                a[poz] = x
                b[poz] = y
        else:
            #adaugam la finalul sirului numarul
            a.append(x)
            b.append(y)

        # Adaugam in listStates listele actuale
        new_a = Logica.make_a_copy(a)
        new_b = Logica.make_a_copy(b)
        while len(listStates) > indiceLista[0]:
            listStates.pop()
        indiceLista[0] += 2
        listStates.append(new_a)
        listStates.append(new_b)
    except:
        print("Input gresit!")

def Modificare(a,b,listStates,txt,indiceLista):  #Comanda 2
    try:
        if txt.find('i') != -1: # inlocuim toate aparițiile unui număr complex cu un alt număr complex
            txt = txt.replace(' ','')
            auxMIN = txt[:]
            auxMIN = auxMIN.replace('i','')
            auxMIN = auxMIN.replace('0','')
            auxMIN = auxMIN.replace('1','')
            auxMIN = auxMIN.replace('2','')
            auxMIN = auxMIN.replace('3','')
            auxMIN = auxMIN.replace('4','')
            auxMIN = auxMIN.replace('5','')
            auxMIN = auxMIN.replace('6','')
            auxMIN = auxMIN.replace('7','')
            auxMIN = auxMIN.replace('8','')
            auxMIN = auxMIN.replace('9','')
            auxMIN = auxMIN.replace(',','')
            nrMinus = txt.count('-')
            if nrMinus > 4: 
                raise ValueError

            minus = [1,1,1,1]
            for i in range(len(auxMIN)):
                if auxMIN[i] == '-':
                    minus[i] = -1
            

            txt = txt.replace('i',' ')
            txt = txt.replace('+',' ')
            txt = txt.replace(',',' ')
            txt = txt.replace('-',' ')

            auxl = txt.split()
            auxl = list(map(int,auxl)) #toate elementele din auxl devin numere intregi
            if len(auxl) > 4:
                raise ValueError
            auxl[0] *= minus[0]
            auxl[1] *= minus[1]
            auxl[2] *= minus[2]
            auxl[3] *= minus[3]
            
            for i in range(len(a)):
                if a[i] == auxl[0] and b[i] == auxl[1]:
                    a[i] = auxl[2]
                    b[i] = auxl[3]
        elif txt.find(',') != -1: 
            # sterge numerele dintr-un interval
            txt = txt.replace(',',' ')
            txt = txt.replace('(',' ')
            txt = txt.replace(')',' ')
            poz1,poz2 = txt.split()
            poz1 = int(poz1)
            poz2 = int(poz2)
            for i in range(poz2 - poz1 + 1):
                del a[poz1]
                del b[poz1]
        else: 
            #stergem elementul de la pozitia poz
            poz = int(txt)
            del a[poz]
            del b[poz] 
        # Adaugam in listStates listele actuale
        new_a = Logica.make_a_copy(a)
        new_b = Logica.make_a_copy(b)
        while len(listStates) > indiceLista[0]:
            listStates.pop()
        indiceLista[0] += 2
        listStates.append(new_a)
        listStates.append(new_b)  

    except:
        print("Input gresit!")

def test_Modificare():
    l = []
    a1 = [1 ,2 ,1 ,5 ,6]
    b1 = [7 ,2 ,7 ,5 ,4]
    Modificare(a1,b1,l,"1+7i , 9+9i",[0])
    assert a1 == [9,2,9,5,6] and b1 == [9,2,9,5,4]
    a2 = [1 ,2 ,1 ,5 ,9 ,11]
    b2 = [7 ,2 ,7 ,5 ,9 ,10]
    Modificare(a2,b2,l,"3",[0])
    assert a2 == [1,2,1,9,11] and b2 == [7,2,7,9,10]
    a3 = [1 ,3 ,1 ,5 ,98 ,12]
    b3 = [1 ,3 ,1 ,5 ,89 ,13]
    Modificare(a3,b3,l,"(1,3)",[0])
    assert a3 == [1 ,98 ,12] and b3 == [1 ,89 ,13]
    a4 = [-1,7,6,-1,-1]
    b4 = [2,7,8,2,3]
    Modificare(a4,b4,l,"-1+2i , -7 -8i",[0])
    assert a4 == [-7,7,6,-7,-1] and b4 == [-8,7,8,-8,3]

def Căutare_Numere(a,b,subcommmand): #Comanda 3
    # in aceasta functie listele nu sunt modificare 
    if subcommmand == '1':
        interval = UI.get_interval()
        UI.afiseaza_interval(a,b,interval)

    elif subcommmand == '2':
        UI.modul_mai_mic_10()
        
    elif subcommmand == '3':
        UI.modul_egal_10()

def Operații(a,b,listStates,subcommand,indiceLista): #Comanda 4
    try:
        if subcommand == 1 or subcommand == 2:
            txt = input("Introduceti intervalul:\n")
            txt = txt.replace(',',' ')
            poz1,poz2 = txt.split()
            poz1 = int(poz1)
            poz2 = int(poz2)
            if subcommand == 1:
                print("Suma subsecventei este :",Logica.Suma_Nr_Complexe(a[poz1:poz2+1],b[poz1:poz2+1]))
            else:
                print("Produsul subsecventei este :",Logica.Produs_Nr_Complexe(a[poz1:poz2+1],b[poz1:poz2+1]))
        elif subcommand == 3:
            #sortare
            Logica.Sortare(a,b)
            string = ""
            for i,j in zip(a,b):
                if j < 0:
                    semn = ''
                else:
                    semn = '+'
                
                string += ' ' + str(i) + semn + str(j) + 'i'
            print(string)
            # Adaugam in listStates listele actuale
            new_a = Logica.make_a_copy(a)
            new_b = Logica.make_a_copy(b)
            while len(listStates) > indiceLista[0]:
                    listStates.pop()
            indiceLista[0] += 2
            listStates.append(new_a)
            listStates.append(new_b)
    except:
        print("Input gresit!")

def Filtrare(a,b,listStates,subcommmand,indiceLista):  #Comanda 5
    if subcommmand == '1' :
        Logica.strege_partea_reala_nr_prim(a,b)
        print("Numerele cu partea reala un numar prim au fost sterse")
    elif subcommmand == '2':
        try:
            txt = input()
            auxl = txt.split()
            numar = int(auxl[1])
            if auxl[0] != '=' and auxl[0] != '<' and auxl[0] != '>' or len(auxl) != 2:
                raise Exception
            Logica.eliminare_modul(a,b,auxl[0],numar)
        except:
            print("Input gresit!")
    
    # Adaugam in listStates listele actuale
    new_a = Logica.make_a_copy(a)
    new_b = Logica.make_a_copy(b)
    while len(listStates) > indiceLista[0]:
            listStates.pop()
    indiceLista[0] += 2
    listStates.append(new_a)
    listStates.append(new_b)

def Undo(a,b,listStates,indiceLista): #Comanda 6
    if indiceLista[0] > 2: 
        indiceLista[0] -= 2
        while len(a) != 0:
            del a[0]
        while len(b) != 0:
            del b[0]
        for el in listStates[indiceLista[0]-1]:
            b.append(el)
        for el in listStates[indiceLista[0]-2]:
            a.append(el)
    elif indiceLista[0] == 2:
        indiceLista[0] -= 2
        while len(a) != 0:
            del a[0]
        while len(b) != 0:
            del b[0]

def Afisare(a,b):  #Comanda 7
    string = ""
    for i,j in zip(a,b):
        semn = '+'
        if j < 0:
            semn = ''
        string += str(i) + semn + str(j) + 'i  '
    print(string)

def Redo(a,b,listStates,indiceLista): # Comanda 10 
    if len(listStates) > indiceLista[0]:
        indiceLista[0] += 2
        while len(a) != 0:
            del a[0]
        while len(b) != 0:
            del b[0]
        for el in listStates[indiceLista[0]-1]:
            b.append(el)
        for el in listStates[indiceLista[0]-2]:
            a.append(el)
#Teste
test_Modificare() 