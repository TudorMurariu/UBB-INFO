

class Console:
    def __init__(self, srv_stud, srv_discipline):
        self.srv_stud = srv_stud
        self.srv_discipline = srv_discipline

    def citeste_student(self):
        ID = input("ID-ul studentului: ")
        nume = input("Numele studentului: ")
        return (ID,nume)

    def citeste_disciplina(self):
        ID = input("ID-ul disciplinei: ")
        nume = input("Numele disciplinei: ")
        profesor = input("Numele profesorului: ")
        return (ID,nume,profesor)

    def open_app(self):
        print("Type help and press enter to see the commands")
        while True:
            command = input("Introdu o comanda: \n")
            args = command.split() # splits the text by the spaces

            if command == "exit" or args[0] == "exit":
                break
            elif command == "help" or args[0] == "help":
                print("Comenzile sunt:")
                print("exit pentru a iesit din aplicatie")
                print("help pentru afisarea comenzilor")
                print("print studenti pentru a afisa lista de studenti")
                print("print discipline pentru a afisa lista de discipline")
                print("adauga student/disciplina pentru a adauga student/disciplina in lista respectiva")
                print("sterge strudent/disciplina prentru a sterge student/disciplina din lista respectiva\n")
            elif args[0] == "print":
                if len(args) == 1:
                    print("Specificati daca vreti sa vedeti lista de studenti sau de discipline.")
                elif args[1] == "studenti":
                    lista_studenti = self.srv_stud.get_studenti()
                    for el in lista_studenti:
                        print(el)
                elif args[1] == "discipline":
                    lista_discipline = self.srv_discipline.get_discipline()
                    for el in lista_discipline:
                        print(el)

            elif args[0] == "adauga":
                if len(args) == 1:
                    print("Specificati daca vreti sa adaugati un student sau o disciplina.")
                elif args[1] == "student":
                    (ID,nume) = self.citeste_student()
                    e = self.srv_stud.adauga_student(ID, nume)
                    if e != "":
                        print(e)
                elif args[1] == "disciplina":
                    (ID,nume,profesor) = self.citeste_disciplina()
                    e = self.srv_discipline.adauga_disciplina(ID, nume, profesor)
                    if e != "":
                        print(e)

            elif args[0] == "sterge":
                if len(args) == 1:
                    print("Specificati daca vreti sa stergeti un student sau o disciplina.")
                elif len(args) == 2:
                    print("Specificati un id.")
                elif args[1] == "student":
                    e = self.srv_stud.sterge_student(args[2])
                    if e != "":
                        print(e)
                elif args[1] == "disciplina":
                    e = self.srv_discipline.sterge_disciplina(args[2])
                    if e != "":
                        print(e)