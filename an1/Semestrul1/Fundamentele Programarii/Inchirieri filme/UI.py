
class Console:
    def __init__(self, srv_filme, srv_clienti):
        self.srv_filme = srv_filme
        self.srv_clienti = srv_clienti

    def citeste_film(self):
        ID = input("ID-ul : ")
        titlu = input("Titlu : ")
        descriere = input("Descrierea : ")
        gen = input("Genul : ")

        return (ID,titlu,descriere,gen)

    def citeste_clienti(self):
        ID = input("ID-ul : ")
        nume = input("Numele : ")
        CNP = input("CNP-ul : ")

        return (ID,nume,CNP)

    def open_app(self):
        print("Type help and then press enter to see the comands.")

        while True:
            command = input("Introdu o comanda:\n")
            args = command.split() # splits by spaces

            if command == "exit" or args[0] == "exit":
                break;
            elif command == "help" or args[0] == "help":
                print("Comenzile sunt:")
                print("exit pentru a inchide aplicatia")
                print("help pentru a vedea comenzile")
                print("adauga film/client pentru a adauga in lista respectiva un element")
                print("sterge film/client pentru a sterge din lista respectiva un element")
                print("print filme/clienti pentru a afisa lista de filme/clienti")

            elif args[0] == "print":
                if len(args) == 1:
                    print("Specificati daca vreti sa afisati filme sau clienti.")
                elif args[1] == "filme":
                        lista_filme = self.srv_filme.get_filme()
                        if len(lista_filme) == 0:
                            print("lista este goala")
                        else:
                            for el in lista_filme:
                                print(el)

                elif args[1] == "clienti":
                        lista_clienti = self.srv_clienti.get_clienti()
                        if len(lista_clienti) == 0:
                            print("lista este goala")
                        else:
                            for el in lista_clienti:
                                print(el)

            elif args[0] == "adauga":
                if len(args) == 1:
                    print("Specificati daca vreti sa adaugati un film sau un client.")
                elif args[1] == "film":
                    ID,titlu,descriere,gen = self.citeste_film()
                    e = self.srv_filme.adauga_film(ID,titlu,descriere,gen)
                    if e != "":
                        print(e)

                elif args[1] == "client":
                    ID,nume,CNP = self.citeste_clienti()
                    e = self.srv_clienti.adauga_client(ID,nume,CNP)
                    if e != "":
                        print(e)

            elif args[0] == "sterge":
                if len(args) == 1:
                    print("Specificati daca vreti sa stergeti un film sau un client.")
                elif len(args) == 2:
                    print("Specificati si un ID.")
                elif args[1] == "film":
                        ID = args[2]
                        e = self.srv_filme.sterge_film(ID)
                        if e != "":
                            print(e)
                elif args[1] == "client":
                        ID = args[2]
                        e = self.srv_clienti.sterge_client(ID)
                        if e != "":
                            print(e)

            elif args[0] == "modifica":
                if len(args) == 1:
                    print("Specificati daca vreti sa modificati un film sau un client.")
                elif len(args) == 2:
                    print("Specificati si un ID.")
                elif args[1] == "film":
                        ID = args[2]
                        print("Alegeti filmul nou:")
                        ID_nou,titlu_nou,descriere_nou,gen_nou = self.citeste_film()
                        e = self.srv_filme.modifica_film(ID,ID_nou,titlu_nou,descriere_nou,gen_nou)
                        if e != "":
                            print(e)
                elif args[1] == "client":
                        ID = args[2]
                        print("Alegeti clientul nou:")
                        ID_nou,nume_nou,CNP_nou = self.citeste_clienti()
                        e = self.srv_clienti.modifica_client(ID,ID_nou,nume_nou,CNP_nou)
                        if e != "":
                            print(e)