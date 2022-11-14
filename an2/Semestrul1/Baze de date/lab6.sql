CREATE PROCEDURE Test
AS
	Select * from Client
GO

USE Bolt_Food

EXEC Test;
GO


---- modifica tipul unuei coloane
CREATE PROCEDURE ChangeClient_nume_complet_to_varchar
AS
	ALTER TABLE Client
	ALTER COLUMN nume_complet varchar(100);
GO

CREATE PROCEDURE ChangeClient_nume_complet_to_nvarchar
AS
	ALTER TABLE Client
	ALTER COLUMN nume_complet nvarchar(100);
GO

EXEC ChangeClient_nume_complet_to_varchar;

EXEC ChangeClient_nume_complet_to_nvarchar;
GO


--adauga o costrângere de “valoare implicită” pentru un câmp
--- adds default value to password
CREATE PROCEDURE Add_Default_Value_Client_parola
AS
	ALTER TABLE Client
	ADD CONSTRAINT df_parola
	DEFAULT '12345' for parola;
GO


CREATE PROCEDURE Delete_Default_Value_Client_parola
AS
	ALTER TABLE Client
	DROP CONSTRAINT df_parola; /* sau ALTER COLUMN City DROP DEFAULT; */
GO

DROP PROCEDURE Add_Default_Value_Client_parola;

EXEC Add_Default_Value_Client_parola;
EXEC Delete_Default_Value_Client_parola;
GO


--creea/şterge o tabelă;
CREATE PROCEDURE Creeaza_O_Tabela
AS
BEGIN
	CREATE TABLE Person(
	Person_ID INT NOT NULL PRIMARY KEY,
	FirstName varchar(50) NOT NULL,
	LastName varchar(50) NOT NULL
	);
END
GO

CREATE PROCEDURE Sterge_Tabela_Noua
AS
BEGIN
	DROP table Person;
END
GO

Creeaza_O_Tabela;
EXECUTE Sterge_Tabela_Noua;
go
--adauga un camp nou

CREATE PROCEDURE Adauga_Camp_Nou
AS
BEGIN
	Alter Table Person
	ADD City varchar
END
GO

CREATE PROCEDURE Sterge_Camp_Nou
AS
BEGIN
	Alter Table Person
	DROP COLUMN City
END
GO

DROP PROCEDURE Sterge_Camp_Nou

Creeaza_O_Tabela;
EXEC Adauga_Camp_Nou;
EXECUTE Sterge_Camp_Nou;
EXEC Sterge_Tabela_Noua;
GO

-- creare/stergere constrangere de cheie straina
CREATE PROCEDURE Creaza_Cheie_Straina
AS
BEGIN
	ALTER TABLE Person
	ADD CONSTRAINT FK_ID_PERSOANA
	FOREIGN KEY (Person_ID) REFERENCES Client(Id_Client);
END
GO

CREATE PROCEDURE Strege_Cheie_Straina
AS
BEGIN
	Alter Table Person
	DROP CONSTRAINT FK_ID_PERSOANA;
END
GO

EXEC Creaza_Cheie_Straina;
EXEC Strege_Cheie_Straina;

-- Cream tabelul cu versiunea Bazei de date
Create Table VersiuneDB(
	Numar_Versiune INT default 0
);

INSERT INTO VersiuneDB
values(0);

SELECT * FROM VersiuneDB;

-- cream doua tabele auxiliara
Create Table Lista_Proceduri_VersiuniDB(
	ID INT PRIMARY KEY,
	nume_procedura varchar(100)
);

INSERT INTO Lista_Proceduri_VersiuniDB
values
(0, 'ChangeClient_nume_complet_to_varchar'),
(1, 'Add_Default_Value_Client_parola'),
(2, 'Creeaza_O_Tabela'),
(3, 'Adauga_Camp_Nou'),
(4, 'Creaza_Cheie_Straina');

Create Table Lista_Opus_Proceduri_VersiuniDB(
	ID INT PRIMARY KEY,
	nume_procedura varchar(100)
);


INSERT INTO Lista_Opus_Proceduri_VersiuniDB
values
(1, 'ChangeClient_nume_complet_to_nvarchar'),
(2, 'Delete_Default_Value_Client_parola'),
(3, 'Sterge_Tabela_Noua'),
(4, 'Sterge_Camp_Nou'),
(5, 'Strege_Cheie_Straina');


SELECT * FROM Lista_Proceduri_VersiuniDB;

GO

-- main
CREATE PROCEDURE main
@Versiune int
AS
BEGIN
	 IF @Versiune>5
	 BEGIN
		 RAISERROR('nu exista decat 6 veriuni ale DB-ul (de la 0 la 5 inclusiv)', 16, 1);
		RETURN;
	 END

	 DECLARE @Versiune_actuala AS INT
	 SELECT @Versiune_actuala = Numar_Versiune
	 FROM VersiuneDB;

	 PRINT 'versiunea actuala este :'; 
	 PRINT @Versiune_actuala;
	 PRINT 'Schimbam la veriunea :'; 
	 PRINT @Versiune;

	 IF @Versiune=@Versiune_actuala
	 BEGIN
		PRINT 'Suntem deja in aceasta versiune!';
		RETURN;
	 END

	 DECLARE @Functie AS varchar(100);


	 IF @Versiune>@Versiune_actuala
	 BEGIN
		WHILE @Versiune!=@Versiune_actuala
		BEGIN

			SELECT @Functie = nume_procedura
			FROM Lista_Proceduri_VersiuniDB
			where @Versiune_actuala=ID;

			EXECUTE @Functie;

			SET @Versiune_actuala=@Versiune_actuala+1;

		END

		UPDATE VersiuneDB
		SET Numar_Versiune = @Versiune;

		RETURN;
	 END
	 
	 -- altfel daca  @Versiune < @Versiune_actuala

	 WHILE @Versiune!=@Versiune_actuala
		BEGIN

			set @Versiune_actuala=@Versiune_actuala-1;

			SELECT @Functie = nume_procedura
			FROM Lista_Opus_Proceduri_VersiuniDB
			where @Versiune_actuala=ID;

			EXECUTE @Functie;
		END
	 
	 UPDATE VersiuneDB
	 SET Numar_Versiune = @Versiune;
	 RETURN;
END

go
EXEC main 0;

DROP PROCEDURE main

UPDATE VersiuneDB
SET Numar_Versiune = 0;