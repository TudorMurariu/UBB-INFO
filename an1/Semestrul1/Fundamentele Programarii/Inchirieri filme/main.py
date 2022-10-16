from UI import Console
from service_clienti import Service_clienti
from service_filme import Service_filme
from clienti_repo import Clienti_repo
from filme_repo import Filme_repo
from validators import Validators

repo_clienti = Clienti_repo()
repo_filme = Filme_repo()
valid = Validators()

service_clienti1 = Service_clienti(repo_clienti, valid)
service_filme1 = Service_filme(repo_filme, valid)

console = Console(service_filme1, service_clienti1)
console.open_app()
