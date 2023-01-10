CREATE DATABASE GAMES;

------------------------------------- creare tabele ----------------------------------------------

CREATE TABLE Company(
	Id_Company int identity primary key,
	name varchar(30) not null,
	description varchar(500) not null,
	website varchar(100) not null
);

CREATE TABLE Game(
	Id_Game int identity primary key,
	name varchar(30) not null,
	realease_date DATE not null,
	Id_Company int foreign  key references Company(Id_Company)
);

CREATE TABLE Cinematic(
	Id_Cinematic int identity primary key,
	name varchar(30) not null,
	Id_Game int foreign  key references Game(Id_Game),
);

CREATE TABLE Hero(
	Id_Hero int identity primary key,
	name varchar(30) not null,
	description varchar(500) not null,
	importance int default 0
);

CREATE TABLE Cinematic_Hero(
	Id_Cinematic int foreign  key references Cinematic(Id_Cinematic),
	Id_Hero int foreign  key references Hero(Id_Hero),
	entry_moment TIME not null
);

---------------------------- insert values -----------------------------------


INSERT INTO Company 
values ('Company1', 'descriere', 'www.company1.com'),
('Company2', 'descriereadesfdrgtrfewsrgd', 'www.company2.com'),
('Company3', 'dfsdgher', 'www.company3.com');


INSERT INTO Game 
values ('Minecraft', '2012', 1);

INSERT INTO Game 
values ('Game2', '2020', 2),
('Lol', '2010', 3),
('Valorant', '2019', 3);

INSERT INTO Cinematic 
values ('Cinematic1', 1),
('Cinematic2', 1),
('Cinematic3', 2),
('Cinematic4', 3),
('Cinematic5', 3),
('Cinematic6', 3);

INSERT INTO Hero 
values ('Steve', 'wdaesfdgrtfh', 100),
('Leona', 'wdaesfdgrtfh', 20),
('Alex', 'wdaesfdgrtfh', 70),
('Kassadin', 'wdaesfdgrtfh', 30),
('Irelia', 'wdaesfdgrtfh', 90),
('Rengar', 'wdaesfdgrtfh', 0);

INSERT INTO Cinematic_Hero
values (4, 2, '12:00'),
(4, 4, '8:00'),
(4, 5, '9:30'),
(1, 1, '3:12'),
(1, 3, '3:12'),
(1, 3, '11:45');

INSERT INTO Cinematic_Hero
values (1, 6, '2:12'),
(2, 6, '2:12'),
(3, 6, '2:12'),
(4, 6, '2:12'),
(5, 6, '2:12'),
(6, 6, '2:12');

select * from Cinematic_Hero;

GO

------------------------------ Procedure ----------------------------------------------




GO
--------------------------------- View --------------------------------------------------------

CREATE OR ALTER View show_heros
as
	select H.name, H.importance
	from Hero as H 
	inner join Cinematic_Hero as CH on H.Id_Hero = CH.Id_Hero
	group by H.name, H.importance
	having ((select count(CH.Id_Hero) as nr) = (select count(*) as Expr1 from Cinematic))
GO

SELECT * FROM show_heros;