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
        txt = input("Introdu o comanda:\n")
        command = txt.split()[0]    # primul cuvant
        
        if command == 'Adauga' or command == 'adauga':
            txt = txt[txt.find('g') + 2:]
            Functii.Citire(a,b,listStates,txt,indiceLista)
        elif command == 'Modificare' or command == 'modificare':
            txt = txt[txt.find('e')+1:]
            Functii.Modificare(a,b,listStates,txt,indiceLista)
        elif command == 'Cautare' or command == 'cautare':
            print("\nAlege o subcomanda :")
            print("     1. Tipărește partea imaginara pentru numerele din listă.Se dă intervalul de poziții (sub secvența).")
            print("     2. Tipărește toate numerele complexe care au modulul mai mic decât 10.")
            print("     3. Tipărește toate numerele complexe care au modulul egal cu 10.")
            subcommand = input()
            Functii.Căutare_Numere(a,b,subcommand)

        elif command == 'Operatii' or command == 'operatii':
            print("\nAlege o subcomanda :")
            print("     1. Suma numerelor dintr-o subsecventă dată (se da poziția de început și sfârșit). ")
            print("     2. Produsul numerelor dintr-o subsecventă dată (se da poziția de început și sfârșit).")
            print("     3. Tipărește lista sortată descrescător după partea imaginara.")
            subcommand = input()
            Functii.Operații(a,b,listStates,subcommand,indiceLista)

        elif command == 'Filtrare' or command == 'filtrare':
            print("\nAlege o subcomanda :")
            print("     1. Filtrare parte reala prim – elimină din listă numerele complexe la care partea reala este prim.")
            print("     2. Filtrare modul – elimina din lista numerele complexe la care modulul este <,= sau > decât un număr dat.")
            subcommmand = input()
            Functii.Filtrare(a,b,listStates,subcommmand,indiceLista)

        elif command == 'Undo' or command == 'undo':
            Functii.Undo(a,b,listStates,indiceLista)
        elif command == 'Print' or command == 'print':
            Functii.Afisare(a,b)
        elif command == 'Comenzi' or command == 'comenzi':
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
        elif command == 'Redo' or command == 'redo':
            Functii.Redo(a,b,listStates,indiceLista);
            #print(listStates)
        elif command == 'Exit' or command == 'exit':
            break
        

if __name__ == '__main__':
    Interfata()


