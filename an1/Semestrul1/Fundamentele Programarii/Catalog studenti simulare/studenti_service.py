from studenti import Studenti

class srv_studenti:
    def __init__(self, valid, repo):
        self.valid = valid
        self.repo = repo

    def get_studenti(self):
        return self.repo.lista_studenti

    def adauga_student(self, ID, nume):
        if not self.valid.verifica_ID(ID):
            return "ID incorect"
        elif not self.valid.verifica_nume(nume):
            return "nume incorect"
        else:
            student = Studenti(ID, nume)
            return self.repo.adauga_student(student)

    def sterge_student(self, ID):
        if not self.valid.verifica_ID(ID):
            return "ID incorect"
        else:
            return self.repo.sterge_student(ID)