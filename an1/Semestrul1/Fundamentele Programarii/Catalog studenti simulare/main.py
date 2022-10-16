from UI import Console
from Validators import Validators
from studenti_service import srv_studenti
from discipline_service import srv_discipline
from studenti_repo import repo_studenti
from discipline_repo import repo_discipline

valid = Validators()
stud_repo = repo_studenti()
disc_repo = repo_discipline()

student_srv = srv_studenti(valid, stud_repo)
discipline_srv = srv_discipline(valid, disc_repo)
console = Console(student_srv, discipline_srv)

console.open_app()