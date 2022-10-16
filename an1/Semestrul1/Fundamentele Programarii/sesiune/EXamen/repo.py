class Repo:
    def __init__(self, lista_spectacole, nume_fisier):
        self.nume_fisier = nume_fisier
        self.lista_spectacole = lista_spectacole

    def adauga_spectacol(self, spectacol):
        # verificam daca spectacolul exista deja
        for el in self.lista_spectacole:
            if el == spectacol:
                return "Spectacolul exista deja, folositi comanda modifica pentru a il modifica"
        
        # daca nu exista deja il adaugam in lista
        self.lista_spectacole.append(spectacol)
        # si il scriem la finalul fisier
        with open(self.nume_fisier,"w") as f:
            for el in self.lista_spectacole:
                f.write(str(el) + "\n")

        return "" #daca nu au aparut erori

    def modifica_spectacol(self, spectacol):
        '''
            Cautam spectacolul 
            Daca il gasim il modificam daca nu returnam mesajul de eroare
        '''
        if len(self.lista_spectacole) == 0:
            return "Nu exista un astfel de spectacol"

        for i in range(len(self.lista_spectacole)):
            if self.lista_spectacole[i] == spectacol:
                self.lista_spectacole[i].set_durata(spectacol.get_durata())
                self.lista_spectacole[i].set_gen(spectacol.get_gen())

                with open(self.nume_fisier,"w") as f:
                    for el in self.lista_spectacole:
                        f.write(str(el) + "\n")
                return ""

        return "Nu exista un astfel de spectacol"
    
    def export(self, nume_fisier):
        '''
            Exportam spectacolele sortate dupa artist si apoi dupa titlu
            in fisierul dat 
        '''
        with open(nume_fisier,"w") as f:
            for el in sorted(self.lista_spectacole):
                f.write(str(el) + "\n")

