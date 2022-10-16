
class repo_discipline:
    def __init__(self):
        self.lista_discipline = []

    def exista_ID(self, ID):
        for el in self.lista_discipline:
            if ID == el.ID:
                return True
        return False

    def adauga_disciplina(self, disciplina):
        if self.exista_ID(disciplina.ID):
            return "Exista deja o disciplina cu acest ID."
        else:
            self.lista_discipline.append(disciplina)
            return ""
        
    def sterge_disciplina(self, ID):
        for i in range(len(self.lista_discipline)):
            if self.lista_discipline[i].ID == ID:
                del self.lista_discipline[i]
                return ""
        return "Nu exista o disciplina cu un astfel de id"