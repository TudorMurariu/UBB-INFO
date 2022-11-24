from clienti import Clienti

class Service_clienti:
    def __init__(self, repo, valid):
        self.repo = repo
        self.valid = valid

    def adauga_client(self, ID, nume, CNP):
        if not self.valid.verifica_ID(ID):
            return "ID incorect"
        elif not self.valid.verifica_nume(nume):
            return "nume incorect"
        elif not self.valid.verifica_CNP(CNP):
            return "CNP incorect"
        
        return self.repo.adauga_client(Clienti(ID, nume, CNP))

    def sterge_client(self, ID):
        if not self.valid.verifica_ID(ID):
            return "ID incorect"
        
        return self.repo.sterge_client(ID)

    def get_clienti(self):
        return self.repo.lista_clienti

    def modifica_client(self, ID,ID_nou,nume_nou,CNP_nou):
        if not self.valid.verifica_ID(ID):
            return "ID incorect"
        elif not self.valid.verifica_ID(ID_nou):
            return "ID_nou incorect"
        elif not self.valid.verifica_nume(nume_nou):
            return "nume_nou incorect"
        elif not self.valid.verifica_CNP(CNP_nou):
            return "CNP_nou incorect"
        
        return self.repo.modifica_client(ID,ID_nou,nume_nou,CNP_nou)


    