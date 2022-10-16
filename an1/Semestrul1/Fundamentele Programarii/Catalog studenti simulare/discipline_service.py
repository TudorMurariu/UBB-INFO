from discipline import Discipline

class srv_discipline:
    def __init__(self, valid, repo):
        self.valid = valid
        self.repo = repo

    def get_discipline(self):
        return self.repo.lista_discipline

    def adauga_disciplina(self, ID, nume, profesor):
        if not self.valid.verifica_ID(ID):
            return "ID incorect"
        elif not self.valid.verifica_nume(profesor):
            return "nume profesor incorect"
        else:
            disciplina = Discipline(ID, nume, profesor)
            return self.repo.adauga_disciplina(disciplina)
    
    def sterge_disciplina(self, ID):
        if not self.valid.verifica_ID(ID):
            return "ID incorect"
        else:
            return self.repo.sterge_disciplina(ID)
