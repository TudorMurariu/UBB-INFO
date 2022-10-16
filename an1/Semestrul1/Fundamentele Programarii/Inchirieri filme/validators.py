
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
        args = nume.split() # splits by spaces

        return len(args) >= 2

    def verifica_CNP(self, CNP):
        try:
            a = int(CNP)
            return True
        except:
            return False