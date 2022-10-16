
class Studenti:
    def __init__(self, ID, nume):
        self.ID = ID
        self.nume = nume

    def __str__(self):
        return "(" + str(self.ID) + ", " + self.nume + ")"