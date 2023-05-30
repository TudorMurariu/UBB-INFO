Create table Cofetarie(
	id int identity PRIMARY KEY,
	nume varchar(100)
);

Create table Briosa(
	id int PRIMARY KEY identity,
	nume varchar(100),
	idC int foreign key REFERENCES  Cofetarie(id)
);

insert into Cofetarie
values 
('Cofetaria Ioana'),
('Gelato'),
('Lifeberry'),
('Sweets Palace');

insert into Briosa
values 
('Briosa1', 1),
('Briosa2', 1),
('Briosa3', 2),
('Briosa4', 2),
('Briosa5', 3),
('Briosa6', 3),
('Briosa7', 3),
('Briosa8', 4);

Select * from Briosa;
Select * from Cofetarie;