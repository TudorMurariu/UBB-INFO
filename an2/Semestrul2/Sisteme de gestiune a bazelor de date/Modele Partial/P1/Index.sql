CREATE INDEX index_nume
on Cofetarie(nume);

Select * from Cofetarie
order by nume;


--- Daca inserati selectul si dati click dreapta
--- o sa va apara optiunea "Display Estimated Execution Plan"
--- acolo puteti vedea daca este folosit indexul.