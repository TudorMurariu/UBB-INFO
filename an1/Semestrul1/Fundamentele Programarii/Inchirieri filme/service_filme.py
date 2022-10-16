from filme import Filme

class Service_filme:
    def __init__(self, repo, valid):
        self.repo = repo
        self.valid = valid
    
    def adauga_film(self, ID, titlu, descriere, gen):
        if not self.valid.verifica_ID(ID):
            return "ID incorect"
        
        return self.repo.adauga_film(Filme(ID, titlu, descriere, gen))
        
    def sterge_film(self, ID):
        if not self.valid.verifica_ID(ID):
            return "ID incorect"
        
        return self.repo.sterge_film(ID)

    def get_filme(self):
        return self.repo.lista_filme

    def modifica_film(self, ID,ID_nou,titlu_nou,descriere_nou,gen_nou):
        if not self.valid.verifica_ID(ID):
            return "ID incorect"
        elif not self.valid.verifica_ID(ID_nou):
            return "ID_nou incorect"

        return self.repo.modifica_film(ID,ID_nou,titlu_nou,descriere_nou,gen_nou)