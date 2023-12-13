class Gramatica:
    def __init__(self):
        self.reguli = {}

    def citeste_gramatica(self, nume_fisier):
        with open(nume_fisier, 'r') as fisier:
            for linie in fisier:
                linie = linie.strip()
                if linie:
                    partile = linie.split('->')
                    neterminal = partile[0].strip()
                    productii = [prod.strip() for prod in partile[1].split('|')]
                    self.reguli[neterminal] = productii

    def afiseaza_regulile(self, neterminal_cautat):
        if neterminal_cautat in self.reguli:
            print(f"Regulile pentru neterminalul {neterminal_cautat} sunt:")
            for productie in self.reguli[neterminal_cautat]:
                print(f"{neterminal_cautat} -> {productie}")
        else:
            print(f"Neterminalul {neterminal_cautat} nu a fost găsit în gramatică.")

# Exemplu de utilizare
gramatica = Gramatica()
nume_fisier_gramatica = 'gramatica.txt'  # înlocuiți cu numele fișierului vostru
gramatica.citeste_gramatica(nume_fisier_gramatica)

neterminal_cautat = input("Introduceți neterminalul căutat: ")
gramatica.afiseaza_regulile(neterminal_cautat)
