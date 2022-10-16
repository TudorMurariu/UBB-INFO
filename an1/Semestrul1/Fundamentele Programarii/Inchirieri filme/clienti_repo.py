
class Clienti_repo:
    def __init__(self):
        self.lista_clienti = []
    
    def adauga_client(self, client):
        for el in self.lista_clienti:
            if client.ID == el.ID:
                return "Exista deja un client cu acest ID"
        
        self.lista_clienti.append(client)
        return ""
    
    def sterge_client(self, ID):
        for i in range(len(self.lista_clienti)):
            if ID == self.lista_clienti[i].ID:
                del self.lista_clienti[i]
                return ""
        return "ID-ul nu exista"

    def modifica_client(self, ID,ID_nou,nume_nou,CNP_nou):
        for i in range(len(self.lista_clienti)):
            if ID == self.lista_clienti[i].ID:
                self.lista_clienti[i].ID = ID_nou
                self.lista_clienti[i].nume = nume_nou
                self.lista_clienti[i].CNP = CNP_nou
                return ""
        return "ID-ul nu exista"