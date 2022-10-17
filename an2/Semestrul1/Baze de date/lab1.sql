CREATE DATABASE Ceaiuri;
go

use Ceaiuri;
go

CREATE TABLE Tipuri(
Tid INT PRIMARY KEY,
Tip VARCHAR(50) NOT NULL,
Descriere VARCHAR(30));


CREATE TABLE Ceaiuri(
Cid INT PRIMARY KEY IDENTITY,
Denumire VARCHAR(30),
Pret FLOAT DEFAULT 10,
Tid INT FOREIGN KEY REFERENCES Tipuri(Tid));

INSERT INTO Tipuri(Tid, Tip, Descriere)
VALUES (13, 'De Musetel', 'foarte fain');
