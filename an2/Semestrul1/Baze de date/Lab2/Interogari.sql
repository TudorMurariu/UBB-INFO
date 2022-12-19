

--- cate comenzi are fiecare restaurant
SELECT R.Nume_Restaurant, COUNT(*)
FROM Restaurant R
INNER JOIN Comanda C
ON R.Id_Meniu = C.Id_Restaurant
group by R.Nume_Restaurant;


-----idk 
SELECT R.Nume_Restaurant, Curier.Vehicul
FROM Restaurant R
INNER JOIN Comanda C
ON C.Id_Restaurant = R.Id_Meniu
INNER JOIN Curier
ON C.Id_Curier = Curier.Id_Curier;

-- where
--- pentru fiecare meniu site-ul si fiecare bautura
SELECT M.Id_Meniu, M.Url_site, B.Denumire, B.Pret
FROM Meniu M, Bauturi B    /* sau inner join */
where M.Id_Meniu = B.Id_Meniu
order by M.Id_Meniu;






----------- Tema

-- where
--- selectam adresa, numele clientului si numele restaurantul pentru clientii care au aceeasi adresa cu un restaurant
SELECT R.Id_Adresa, C.nume_complet as nume_client, R.Nume_Restaurant
FROM Client C, Restaurant R
where C.Id_Adresa_actuala = R.Id_Adresa or C.Id_Adresa = R.Id_Adresa;

-- nimic special
--- clientii care sunt si curieri
SELECT C.nume_complet as nume_client, Curier.nume_complet as nume_curier
FROM Client C
FULL OUTER JOIN Curier
ON Curier.nume_complet = C.nume_complet;

-- n-m
-- macar 3 tabele
-- group by
-- having
--- companiile de carduri si numarul de carduri pentru companiile cu mai mult de doua carduri 
SELECT Card_.Companie, COUNT(*) as Numar_Carduri
FROM Card_
INNER JOIN Card_Actual 
ON Card_Actual.Id_Card = Card_.Id_Card
INNER JOIN Client
ON Client.Id_Client = Card_Actual.Id_Client
group by Card_.Companie
having COUNT(Client.parola) > 2;

-- macar 3 tabele
--- Fiecare fel de mancare de la fiecare restaurant ordonat dupa id
SELECT R.Nume_Restaurant, M.Url_site, F.Denumire, F.Pret
FROM Meniu M
INNER JOIN Fel_De_Mancare F  
ON M.Id_Meniu = F.Id_Meniu
INNER JOIN Restaurant R
ON R.Id_Meniu = M.Id_Meniu
order by M.Id_Meniu;

-- macar 3 tabele
-- where
------ fiecare comanda cu pretul mai mare sau egal cu 30 de lei
SELECT Client.nume_complet as Nume_Client, Restaurant.Nume_Restaurant, Curier.nume_complet as Nume_Curier, Curier.Vehicul
FROM Comanda C
INNER JOIN Client
ON C.Id_Client = Client.Id_Client
INNER JOIN Restaurant
ON C.Id_Restaurant = Restaurant.Id_Meniu
INNER JOIN Curier
ON C.Id_Curier = Curier.Id_Curier
INNER JOIN Tip_Vehicul
ON Tip_Vehicul.Denumire = Curier.Vehicul
where C.Pret >= 30;

-- distinct
-- where
--- selectam compania si tipul cardului pentru cardurile distincte care au data expirarii mai tarzie de 2025-09-11
Select DISTINCT C.Companie, C.Tip
FROM Card_ C
where C.Data_De_Expirare > '2025-09-11';

-- n-m
-- macar 3 tabele
-- where
--- Pentru fiecare card de tipul 'Credit' toate adresele clientilor distincte ordonate crescator
SELECT DISTINCT C.Companie, Client.Id_Adresa_actuala
FROM Card_ C
INNER JOIN Card_Actual
on C.Id_Card = Card_Actual.Id_Card
INNER JOIN Client
on Card_Actual.Id_Client = Client.Id_Client
where C.Tip = 'Credit'
ORDER BY Client.Id_Adresa_actuala;

-- group by
-- n-m
-- macar 3 tabele
--- in medie cat de lungi sunt parolele clientilor cu un card de la o anumita companie
SELECT Card_.Companie, AVG(LEN(Client.parola))
FROM Card_
INNER JOIN Card_Actual 
ON Card_Actual.Id_Card = Card_.Id_Card
INNER JOIN Client
ON Client.Id_Client = Card_Actual.Id_Client
group by Card_.Companie;


-- group by
-- haviung
-- macar 3 tabele
-- where
--- pentru fiecare vehicul cu greutatea mai mica de o tona numarul de comenzi
SELECT Tip_Vehicul.Denumire, COUNT(*) as Numar_Comenzi
FROM Curier
INNER JOIN Tip_Vehicul
ON Tip_Vehicul.Denumire = Curier.Vehicul
INNER JOIN Comanda
ON Curier.Id_Curier = Comanda.Id_Curier
where Tip_Vehicul.Tonaj < 1
group by Tip_Vehicul.Denumire
having COUNT(*) > 1;


-- distinct
-- group by
-- having
-- macar 3 tabele
/*Cu cate tipuri de vehicule a fost livrata mancare de la restaurante 
pentru restaurantele cu mai mult de un vechicul*/
SELECT R.Nume_Restaurant, COUNT(DISTINCT Curier.Vehicul) as Vehicule 
FROM Restaurant R
INNER JOIN Comanda C
ON C.Id_Restaurant = R.Id_Meniu
INNER JOIN Curier
ON C.Id_Curier = Curier.Id_Curier
group by R.Nume_Restaurant
having COUNT(DISTINCT Curier.Vehicul) > 1;
