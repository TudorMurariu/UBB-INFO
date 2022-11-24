
class Filme_repo:
    def __init__(self):
        self.lista_filme = []
    
    def adauga_film(self, film):
        for el in self.lista_filme:
            if film.ID == el.ID:
                return "Exista deja un film cu acest ID"
        
        self.lista_filme.append(film)
        return ""
    
    def sterge_film(self, ID):
        for i in range(len(self.lista_filme)):
            if ID == self.lista_filme[i].ID:
                del self.lista_filme[i]
                return ""
        return "ID-ul nu exista"

    def modifica_film(self, ID,ID_nou,titlu_nou,descriere_nou,gen_nou):
        for i in range(len(self.lista_filme)):
            if ID == self.lista_filme[i].ID:
                self.lista_filme[i].ID = ID_nou
                self.lista_filme[i].titlu = titlu_nou
                self.lista_filme[i].descriere = descriere_nou
                self.lista_filme[i].gen = gen_nou
                return ""
        return "ID-ul nu exista"