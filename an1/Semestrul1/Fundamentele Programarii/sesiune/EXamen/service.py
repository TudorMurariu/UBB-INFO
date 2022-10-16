from random import Random, random
from spectacole import Spectacole
import string

def generare_cuv(len):
    r = Random()
    alfabet = string.ascii_lowercase
    alfabet_mare = string.ascii_uppercase
    vocale = "aeiou"
    este_vocala = False
    cuv = ""
    l = 25
    x1 = r.randint(0,l)
    cuv = cuv + alfabet_mare[x1]
    if cuv[0] in vocale:
        este_vocala = True

    for i in range(len-1):
        x1 = r.randint(0,l)
        cuv = cuv + alfabet[x1]
        if cuv[-1] in vocale:
            este_vocala = True

    if not este_vocala: # daca nu exista vocala in cuvant
        cuv = vocale[r.randint(0,4)] + cuv[1:]

    return cuv

def generare_text_cerut():
    r = Random()
    lungime_totala = r.randint(9 , 12)
    lungime_1 = r.randint(1,lungime_totala-2)
    lungime_2 = lungime_totala - lungime_1 - 1
    # avem nevoie si de un space
    cuv1 = generare_cuv(lungime_1)
    cuv2 = generare_cuv(lungime_2)
    return cuv1 + " " + cuv2

def gen_la_intamplare():
    '''
        Generam un gen random
    '''
    r = Random()
    x = r.randint(0 , 3)
    lista_genuri = ["Comedie", "Concert", "Balet", "Altele"]
    return lista_genuri[x]

def durata_la_intamplare():
    '''
        Generam o durata random
    '''
    r = Random()
    x = r.randint(1 , 9999999)
    return x

class Service:
    def __init__(self, repo, valid):
        self.repo = repo
        self.valid = valid

    def adauga_spectacol(self,titlu,artist,gen,durata):
        '''
            Verificam daca inputul e valid apoi trimitem spectacolul
            in service pentru a fi adaugat daca nu exista deja.
        '''

        eroare = ""
        if not self.valid.is_artist(artist):
            eroare += "Artistul este vid\n"
        if not self.valid.is_gen(gen):
            eroare += "Nu exista acest gen\n"
        if not self.valid.is_int(durata):
            eroare += "Durata trebuie sa fie un numar intreg pozitiv\n"

        if eroare != "":
            return eroare

        durata = int(durata)
        spectacol = Spectacole(titlu,artist,gen,durata)
        eroare = self.repo.adauga_spectacol(spectacol)

        return eroare


    def modifica_spectacol(self,titlu,artist,gen,durata):
        '''
            Verificam daca spectacolul citit respecta cerintele
            daca satisface il trimitem in repo
        '''

        eroare = ""
        if not self.valid.is_artist(artist):
            eroare += "Artistul este vid\n"
        if not self.valid.is_gen(gen):
            eroare += "Nu exista acest gen\n"
        if not self.valid.is_int(durata):
            eroare += "Durata trebuie sa fie un numar intreg pozitiv\n"

        if eroare != "":
            return eroare
            
        durata = int(durata)
        spectacol = Spectacole(titlu,artist,gen,durata)
        eroare = self.repo.modifica_spectacol(spectacol)

        return eroare

    def genereaza(self, x):
        '''
            Generam x spectacole si le trimitem in repo pentru a fi adaugate pe rand
        '''

        for i in range(x):

            titlu = generare_text_cerut()
            artist = generare_text_cerut()
            gen = gen_la_intamplare()
            durata = durata_la_intamplare()
            spectacol = Spectacole(titlu,artist,gen,durata)
            self.repo.adauga_spectacol(spectacol)
        
        return ""

    def export(self, nume_fisier):
        '''
            Verificam daca fisierul se termina in txt
            daca da atunci il trimitem in repo
        '''
        if not self.valid.is_fiser:
            return "Fisierele trebuie sa se termine in .txt"
        
        self.repo.export(nume_fisier)
        return ""
