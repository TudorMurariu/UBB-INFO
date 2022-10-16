
class repo_studenti:
    def __init__(self):
        self.lista_studenti = []
        pass
    
    def exista_ID(self, ID):
        for el in self.lista_studenti:
            if ID == el.ID:
                return True
        return False

    def adauga_student(self, student):
        if self.exista_ID(student.ID):
            return "Exista deja o disciplina cu acest ID."
        else:
            self.lista_studenti.append(student)
            return ""

    def sterge_student(self, ID):
        for i in range(len(self.lista_studenti)):
            if self.lista_studenti[i].ID == ID:
                del self.lista_studenti[i]
                return ""
        return "Nu exista un student cu un astfel de id"
