from xmlrpc.client import TRANSPORT_ERROR


class Spectacole:
    def __init__(self, titlu, artist, gen, durata):
        self.titlu = titlu
        self.artist = artist
        self.gen = gen
        self.durata = durata

    def get_gen(self):
        return self.gen
    
    def get_durata(self):
        return self.durata

    def set_gen(self, gen):
        self.gen = gen

    def set_durata(self, durata):
        self.durata = durata

    def __eq__(self, other):
        return self.titlu == other.titlu and self.artist == other.artist

    def __str__(self):
        return str(self.artist) +  " " + str(self.titlu) + " " + str(self.gen) + " " + str(self.durata)

    def __gt__(self, other):
        if self.artist > other.artist:
            return True
        elif self.artist == other.artist and self.titlu > other.titlu:
            return True

def test_Spectacole():
    a = Spectacole("1","2","3",1000)
    assert a == Spectacole("1","2","3",1000)
    b = Spectacole("5","5","5",320)
    assert b == Spectacole("5","5","5",320)
    c = Spectacole("1234","31245","3124",1)
    assert c == Spectacole("1234","31245","3124",1)

    assert str(a) == "2 1 3 1000"
    # am ales tipul acesta de afisare. (artist apoi titlu apoi gen si durata)

test_Spectacole()