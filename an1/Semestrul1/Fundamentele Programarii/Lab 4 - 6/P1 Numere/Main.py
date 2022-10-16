import Comenzi as Functii

a = [] # partea reala
b = [] # partea imaginara
listStates = [ [] , [] ] 
indiceLista = [0]

def Interfata():
    print("Comenzile sunt :")
    print("1. Adaugă număr în listă.")
    print("2. Modifică elemente din listă.")
    print("3. Căutare numere.")
    print("4. Operații cu numerele din listă")
    print("5. Filtrare.")
    print("6. Undo")
    print("7. Afiseaza numerele din lista in consola.")
    print("8. Afiseaza toate comenzile in consola.")
    print("9. Redo")
    print("10. Iesire din aplicatie.\n")

    while True:
        command = input("Introdu o comanda:\n")
        
        if command == '1':
            txt = input("Introdu numarul :\n")
            Functii.Citire(a,b,listStates,txt,indiceLista)
        elif command == '2':
            print("\nIntroduceti input pentru una dintre urmatoarele functionalitati :")
            print("Șterge element de pe o poziție dată. Input de forma : \"POZITIE\" ")
            print("Șterge elementele de pe un interval de poziții. Input de forma : \"(POZITIE1 , POZITIE1)\" ")
            print("Înlocuiește toate aparițiile unui număr complex cu un alt număr complex. Input de forma : \"ELEMENT1 , ELEMENT2\" ")

            txt = input()
            Functii.Modificare(a,b,listStates,txt,indiceLista)
        elif command == '3':
            print("\nAlege o subcomanda :")
            print("     1. Tipărește partea imaginara pentru numerele din listă.Se dă intervalul de poziții (sub secvența).")
            print("     2. Tipărește toate numerele complexe care au modulul mai mic decât 10.")
            print("     3. Tipărește toate numerele complexe care au modulul egal cu 10.")
            subcommand = input()
            Functii.Căutare_Numere(a,b,subcommand)

        elif command == '4':
            print("\nAlege o subcomanda :")
            print("     1. Suma numerelor dintr-o subsecventă dată (se da poziția de început și sfârșit). ")
            print("     2. Produsul numerelor dintr-o subsecventă dată (se da poziția de început și sfârșit).")
            print("     3. Tipărește lista sortată descrescător după partea imaginara.")
            subcommand = input()
            Functii.Operații(a,b,listStates,subcommand,indiceLista)

        elif command == '5':
            print("\nAlege o subcomanda :")
            print("     1. Filtrare parte reala prim – elimină din listă numerele complexe la care partea reala este prim.")
            print("     2. Filtrare modul – elimina din lista numerele complexe la care modulul este <,= sau > decât un număr dat.")
            subcommmand = input()
            Functii.Filtrare(a,b,listStates,subcommmand,indiceLista)

        elif command == '6':
            Functii.Undo(a,b,listStates,indiceLista)
        elif command == '7':
            Functii.Afisare(a,b)
        elif command == '8':
            print("Comenzile sunt :")
            print("1. Adaugă număr în listă.")
            print("2. Modifică elemente din listă.")
            print("3. Căutare numere.")
            print("4. Operații cu numerele din listă")
            print("5. Filtrare.")
            print("6. Undo")
            print("7. Afiseaza numerele din lista in consola.")
            print("8. Afiseaza toate comenzile in consola.")
            print("9. Redo")
            print("10. Iesire din aplicatie.\n")
        elif command == '9':
            Functii.Redo(a,b,listStates,indiceLista);
            #print(listStates)
        elif command == '10':
            break
        

if __name__ == '__main__':
    Interfata()


