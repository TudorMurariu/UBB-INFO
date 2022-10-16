from UI import console
from service import Service
from repo import Repo
from validators import Validators

lista_ = []
nume_fisier_spectacole = "spectacole.txt"

valid = Validators()
repo = Repo(lista_, nume_fisier_spectacole)
service = Service(repo, valid)

console1 = console(service, lista_)
console1.run()