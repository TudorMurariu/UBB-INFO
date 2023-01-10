CREATE DATABASE MagazineDB;
USE MagazineDB;
------------------------------------- creare tabele ----------------------------------------------

CREATE TABLE Clienti(
	Id_Clienti int identity primary key,
	nume varchar(30) not null,
	prenume varchar(30) not null,
	gen varchar(30) not null,
	data_nastere DATE not null
);

CREATE TABLE Produse_Favorite(
	Id_Produse_Favorite int identity primary key,
	denumire varchar(30) not null,
	pret float,
	reducere float,
	Id_Clienti int foreign key references Clienti(Id_Clienti)
);

CREATE TABLE Locatii(
	Id_Locatii int identity primary key,
	localitate varchar(30) not null,
	strada varchar(30) not null,
	numar int default 1,
	cod_postal varchar(10) not null
);

CREATE TABLE Magazine(
	Id_Magazine int identity primary key,
	denumire varchar(30) not null,
	an int default 2022,
	Id_Locatie int foreign key references Locatii(Id_Locatii)
);

CREATE TABLE Magazine_Clienti(
	Id_Magazine int foreign key references Magazine(Id_Magazine),
	Id_Clienti int foreign key references Clienti(Id_Clienti),
	data_cumparaturilor DATE not null,
	pretul_achitat float
	CONSTRAINT pk_Magazine_Clienti PRIMARY KEY (Id_Magazine, Id_Clienti)
);

---------------------------- insert values -----------------------------------


INSERT INTO  Locatii
values ('Targu Neamt', 'Corbului', 17, '645432'),
('Targu Neamt', 'Umbrei', 44, '645432'),
('Piatra Neamt', 'aefstrdy', 2, '645432');

INSERT INTO Magazine
values ('Soridor', '2001', 1),
('Geneza', '2005', 2),
('Kaufland', '2018', 3);

INSERT INTO  Clienti
values ('Varvara', 'Andrei', 'm', '2004'),
('State', 'Alexandru', 'm', '2002'),
('Murariu', 'Emlia', 'f', '5/14/2007');

INSERT INTO Produse_Favorite
values ('Paine', 2, 0, 1),
('Produs2', 17, 0, 1),
('Produs3', 2, 0, 1),
('Produs4', 2, 0, 1),
('Produs5', 2, 0, 2),
('Produs6', 2, 0, 3);

INSERT INTO Magazine_Clienti
values (1, 3, '2022', 37);

SELECT * FROM Magazine;
SELECT * FROM Clienti;
SELECT * FROM Magazine_Clienti;

GO
------------------------------------ Procedura --------------------------------------------------

CREATE OR ALTER PROCEDURE Add_Client_Magazin
	@Id_Magazin int,
	@Id_Client int,

	@data_cumparaturi DATE,
	@pret_achitat float
AS
	if EXISTS(SELECT MC.Id_Clienti from Magazine_Clienti as MC where MC.Id_Clienti = @Id_Client and MC.Id_Magazine = @Id_Magazin)
	BEGIN
		UPDATE Magazine_Clienti
		SET data_cumparaturilor = @data_cumparaturi, pretul_achitat = @pret_achitat
		where Id_Clienti = @Id_Client and Id_Magazine = @Id_Magazin;
	END
	else
	BEGIN
		INSERT INTO Magazine_Clienti
		values (@Id_Magazin, @Id_Client, @data_cumparaturi, @pret_achitat);
	END
GO

EXEC Add_Client_Magazin 1, 3, '2023', 55;

EXEC Add_Client_Magazin 3, 2, '2014', 8;
SELECT * FROM Magazine_Clienti;

GO
--------------------------------------- VIEW -----------------------------------------------

CREATE OR ALTER VIEW Afiseaza_Clienti
as
	select C.nume from Clienti as C
	INNER JOIN Produse_Favorite as P
	on C.Id_Clienti = P.Id_Clienti
	group by C.nume
	having COUNT(*) <= 3;
go

SELECT * FROM Afiseaza_Clienti;