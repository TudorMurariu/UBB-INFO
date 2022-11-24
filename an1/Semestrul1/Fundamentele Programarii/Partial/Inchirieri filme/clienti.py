
class Clienti:
    def __init__(self, ID, nume, CNP):
        self.ID = ID
        self.nume = nume
        self.CNP = CNP


    def __str__(self):
        return "(" + self.ID + ", " + self.nume + ", " + self.CNP + ")"

