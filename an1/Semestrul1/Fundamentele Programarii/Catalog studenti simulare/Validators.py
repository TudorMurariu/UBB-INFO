
class Validators:
    def __init__(self):
        pass

    def verifica_ID(self, ID):
        try:
            a = int(ID)
            return True
        except:
            return False

    def verifica_nume(self, nume):
        cuvinte = nume.split() # splits by spaces
        return len(cuvinte) >= 2
            