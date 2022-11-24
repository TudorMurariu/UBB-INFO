
class Filme:
    def __init__(self, ID, titlu, descriere, gen):
        self.ID = ID
        self.titlu = titlu
        self.descriere = descriere
        self.gen = gen

    def __str__(self):
        return "(" + self.ID + ", " + self.titlu + ", " + self.descriere + ", " + self.gen + ")"

