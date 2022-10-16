class Validators:
    def __init__(self):
        pass

    def is_int(self, durata):
        '''
            Verificam daca durata este numar interg pozitiv
        '''
        try:
            x = int(durata)
            return x > 0
        except:
            return False

    def is_gen(self,gen):
        genuri = ["Comedie", "Concert", "Balet", "Altele"]
        return gen in genuri

    def is_artist(self,artist):
        return artist != None and artist != ""

    def is_fiser(self,fisier):
        return fisier.endswith(".txt")


def test_is_int(valid):
    durata = "123432543"
    assert valid.is_int(durata) == True

    durata2 = ""
    assert valid.is_int(durata2) == False

    durata = "12h5"
    assert valid.is_int(durata) == False

    durata = "ASDFW2s"
    assert valid.is_int(durata) == False

    durata = "2"
    assert valid.is_int(durata) == True

    durata = "-2"
    assert valid.is_int(durata) == False

def test_is_gen(valid):
    gen = "Comedie"
    assert valid.is_gen(gen) == True

    gen2 = "Concert"
    assert valid.is_gen(gen2) == True

    gen = "Balet"
    assert valid.is_gen(gen) == True

    gen = "Altele"
    assert valid.is_gen(gen) == True

    gen = " Comedie    "
    assert valid.is_gen(gen) == False

    gen = "Altele1"
    assert valid.is_gen(gen) == False

    gen = "1234"
    assert valid.is_gen(gen) == False

def test_is_artist(valid):
    artist1 = None
    assert valid.is_artist(artist1) == False

    artist1 = "sadfdsawdes"
    assert valid.is_artist(artist1) == True

    artist1 = ""
    assert valid.is_artist(artist1) == False


def test():
    valid = Validators()
    test_is_int(valid)
    test_is_gen(valid)
    test_is_artist(valid)
    
test()