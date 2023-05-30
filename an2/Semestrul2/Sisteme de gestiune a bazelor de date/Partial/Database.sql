create database S5
go
use S5
go

create table Facultati(
Fid int primary key identity,
Denumire varchar(50),
AnInfiintare int,
Oras varchar(50))

create table Profesori(
Pid int primary key identity,
Nume varchar(50),
Prenume varchar(50),
Titulatura varchar(50),
Gen varchar(20),
DataNastere date,
Fid int foreign key references Facultati(Fid))

create table TipuriArticole(
Tid int primary key identity,
Tip varchar(50),
Categorie varchar(50) CHECK(Categorie IN ('A', 'B', 'C', 'D', 'BDI', 'N')),
Descriere varchar(50))

create table Articole(
Aid int primary key identity,
Titlu varchar(50) NOT NULL,
NrAutori int,
NrPagini int,
AnPublicare int,
Tid int foreign key references TipuriArticole(Tid))

create table Publicatii(
Aid int foreign key references Articole(Aid),
Pid int foreign key references Profesori(Pid),
DataAcceptare date,
Taxa int,
constraint pk_Publicatii primary key(Aid, Pid))

-- 1-n: TipuriArticole-Articole

insert into Facultati values('Facultatea de Fizica', 1980, 'Cluj-Napoca'),
('Facultatea de Medicina', 1950, 'Cluj-Napoca')

insert into Profesori values('Popescu', 'Dan', 'lector univ. dr.', 'M', '12/09/1970', 1),
('Antonescu', 'Ana', 'conferentiar univ. dr.', 'F', '06/12/1982', 1)

insert into TipuriArticole values('Cercetare', 'A', 'Fizica moleculara'), 
('Inovatie', 'B', 'Cuantica')

insert into Articole values('Abordarea moleculara din principii de baza', 4, 12, 2018, 1), 
('Cuantica fluidelor in aplicatii de laborator', 2, 9, 2009, 2)

insert into Publicatii values(1, 1, '07/05/2017', 300),
(2, 2, '02/02/2009', 150)

select * from Facultati
select * from Profesori
select * from TipuriArticole
select * from Articole
select * from Publicatii