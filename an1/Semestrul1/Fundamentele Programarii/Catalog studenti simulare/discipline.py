
class Discipline:
    def __init__(self, ID, nume, profesor):
        self.ID = ID
        self.nume = nume
        self.profesor = profesor

    def __str__(self):
        return "(" + str(self.ID) + ", " + self.nume + ", " + self.profesor + ")"