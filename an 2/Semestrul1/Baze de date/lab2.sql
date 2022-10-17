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

CREATE TABLE Curier(
Id_Curier INT PRIMARY KEY IDENTITY,
nume_complet nvarchar(100) NOT NULL,
email nvarchar(100) NOT NULL,
parola nvarchar(100) NOT NULL,
Denumire nvarchar(100) FOREIGN KEY REFERENCES Tip_Vehicul(Denumire)
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
Tara nvarchar(100) NOT NULL,
Cod_Postal varchar(6)  NOT NULL,
Strada nvarchar(100) NOT NULL,
Numar int,
Nr_apartament int,
CONSTRAINT pk_adr PRIMARY KEY (Tara, Cod_Postal, Strada, Numar, Nr_apartament) -- IDKKKKKKK
);

CREATE TABLE Meniu(
Id_Meniu INT PRIMARY KEY IDENTITY,
Url_site nvarchar(100),
);


CREATE TABLE Bauturi(

);

