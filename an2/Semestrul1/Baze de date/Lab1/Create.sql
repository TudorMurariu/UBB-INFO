-- we create the DB
CREATE DATABASE Bolt_Food;
GO

-- we enter it
USE Bolt_Food;
GO

CREATE TABLE Tip_Vehicul(
Denumire nvarchar(100) PRIMARY KEY,
Tonaj INT DEFAULT 1
);

CREATE TABLE Card_(
Id_Card INT PRIMARY KEY IDENTITY,
Companie nvarchar(100) NOT NULL,
Denumire nvarchar(100) NOT NULL,
Cod_plata varchar(16) NOT NULL,
Tip nvarchar(100) NOT NULL,
Data_De_Expirare DATE NOT NULL,
);

CREATE TABLE Adresa(
Id_Adresa INT PRIMARY KEY IDENTITY,
Tara nvarchar(100) NOT NULL,
Cod_Postal varchar(6)  NOT NULL,
Strada nvarchar(100) NOT NULL,
Numar int,
Nr_apartament int,
-- CONSTRAINT pk_adr PRIMARY KEY (Tara, Cod_Postal, Strada, Numar, Nr_apartament) -- IDKKKKKKK
);

CREATE TABLE Meniu(
Id_Meniu INT PRIMARY KEY IDENTITY,
Url_site nvarchar(100),
);

CREATE TABLE Bauturi(
Id_Bautura INT PRIMARY KEY IDENTITY,
Id_Meniu INT FOREIGN KEY REFERENCES Meniu(Id_Meniu),
Cantitate FLOAT DEFAULT 0,
Denumire nvarchar(100) NOT NULL,
Pret FLOAT DEFAULT 0,
Descriere nvarchar(1000)
);

CREATE TABLE Fel_De_Mancare(
Id_Mancare INT PRIMARY KEY IDENTITY,
Id_Meniu INT FOREIGN KEY REFERENCES Meniu(Id_Meniu),
Cantitate FLOAT DEFAULT 0,
Denumire nvarchar(100) NOT NULL,
Pret FLOAT DEFAULT 0,
Descriere nvarchar(1000)
);

CREATE TABLE Curier(
Id_Curier INT PRIMARY KEY IDENTITY,
nume_complet nvarchar(100) NOT NULL,
email nvarchar(100) NOT NULL,
parola nvarchar(100) NOT NULL,
Vehicul nvarchar(100) FOREIGN KEY REFERENCES Tip_Vehicul(Denumire)
);

CREATE TABLE Client(
Id_Client INT PRIMARY KEY IDENTITY,
nume_complet nvarchar(100) NOT NULL,
email nvarchar(100) NOT NULL,
parola nvarchar(100) NOT NULL,
Id_Adresa_actuala INT FOREIGN KEY REFERENCES Adresa(Id_Adresa),
Id_Adresa INT FOREIGN KEY REFERENCES Adresa(Id_Adresa),
--Id_Card INT FOREIGN KEY REFERENCES Card_(Id_Card)
);

--Drop table Card_Actual

CREATE TABLE Card_Actual(
Id_Client INT FOREIGN KEY REFERENCES Client(Id_Client),
Id_Card INT FOREIGN KEY REFERENCES Card_(Id_Card),
CONSTRAINT pk_card_actual PRIMARY KEY (Id_Client, Id_Card)
);

CREATE TABLE Restaurant(
Nume_Restaurant nvarchar(300) NOT NULL,
Id_Meniu INT FOREIGN KEY REFERENCES Meniu(Id_Meniu) PRIMARY KEY,
Id_Adresa INT FOREIGN KEY REFERENCES Adresa(Id_Adresa)
);

CREATE TABLE Comanda(
Id_Comanda INT PRIMARY KEY IDENTITY,
Id_Client INT FOREIGN KEY REFERENCES Client(Id_Client),
Id_Restaurant INT FOREIGN KEY REFERENCES Restaurant(Id_Meniu),
Id_Curier INT FOREIGN KEY REFERENCES Curier(Id_Curier),
Denumire nvarchar(300) NOT NULL,
Pret FLOAT DEFAULT 0,
Durata_Aproximativa_Livrare Float DEFAULT 30,
Data_Comenzii DATETIME DEFAULT GETDATE()
);

/*
USE master
GO
DROP DATABASE Bolt_Food
*/

SELECT * FROM Card_;