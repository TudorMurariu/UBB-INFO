def citeste_spectacol():
    # citim de la tastatura componentele unui spectacol ca string
    titlu = input("Titlul este: ")
    artist = input("Artistul este: ")
    gen = input("Genul este: ")
    durata = input("Durata este: ")
    return titlu,artist,gen,durata

class console():
    def __init__(self, service, lista_sectacole):
        self.service = service
        self.lista_sectacole = lista_sectacole

    def run(self):
        '''
            Aceasta functie afiseaza consola
            si controleaza partea de comenziprimite de la tastatura
        '''
        print("Scrieti help si apasati enter pentru a vedea comenzile.")
        while True:
            input_ = input("Scrie o comanda: ")
            args = input_.split() # splits by spaces

            if input_ == "exit":
                break
            elif input_ == "help":
                print("Comenzile sunt :")
                print("adauga  pentru a adauga un spectacol(dupa ce apelati comanda veti citi spectacolul)")
                print("modifica  pentru a modifica un spectacol din lista")
                print("genereaza x  pentru a genera si adauga un numar ales x de spectacole la intamplare")
                print("export x.txt  pentru a exporta in fisierul x.txt spectacolele ordonate dupa artist si titlu")
                print("exit  pentru a inchide programul")
                print("help  pentru a vedea toate comenzile")
            elif input_ == "adauga":
                # citim un spectacol si apelam la service
                titlu,artist,gen,durata = citeste_spectacol()
                eroare = self.service.adauga_spectacol(titlu,artist,gen,durata)
                if eroare == "":
                    print("Elementul a fost adaugat cu succes")
                else:
                    print("Eroare!")
                    print(eroare)

            elif input_ == "modifica":
                titlu,artist,gen,durata = citeste_spectacol()
                eroare = self.service.modifica_spectacol(titlu,artist,gen,durata)

                if eroare == "":
                    print("Elementul a fost modificat cu succes")
                else:
                    print("Eroare!")
                    print(eroare)

            elif args[0] == "genereaza":
                if len(args) == 1:
                    print('Specificati cate specacole trebuie generate.')
                elif len(args) > 2:
                    print('Prea multe argumente.')
                else:
                    try:
                        x = int(args[1])
                        self.service.genereaza(x)
                    except:
                        print("x trebuie sa fie un numar intreg")

            elif args[0] == "export":
                if len(args) == 1:
                    print('Specificati fisierul in care trebuie exportat.')
                elif len(args) > 2:
                    print('Prea multe argumente.')
                else:
                    eroare = self.service.export(args[1])

                    if eroare == "":
                        print("Exportat cu succes")
                    else:
                        print("Eroare!")
                        print(eroare)
            
            else:
                print("Nu exista aceasta comanda")
            
    