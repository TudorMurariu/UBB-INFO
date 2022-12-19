/*
	Card : 1 PK, no FK
	Bauturi : 1 PK + FK
	Card_actual : 2 PK
*/

--- Table Tables – with the names of the tables considered

USE Bolt_Food

Insert into Tables(Name)
values ('Card'),
('Bauturi'),
('Card_actual');

--- Table Views – with the names of the views considered (not necessarily created on the tables
-- considered in the table Tables, but preferable to see the time results)
SELECT * FROM Tables;
GO

-- pentru un PK
CREATE VIEW View_Card AS
 SELECT *
 FROM Card_;
GO

-- pentru un PK si cel putin un FK
CREATE VIEW View_Bauturi AS
 SELECT m.Url_site, b.Denumire
 FROM Bauturi b
 Inner Join Meniu m
 ON b.Id_Meniu = m.Id_Meniu;
GO

-- pentru doua PK
CREATE VIEW View_Card_actual AS
 SELECT cl.nume_complet, c.Denumire, c.Companie
 FROM Card_ c
 INNER JOIN Card_actual ca
 ON c.Id_Card = ca.Id_Card
 INNER JOIN Client cl
 ON cl.Id_Client = ca.Id_Client
 group by cl.Id_Client,  cl.nume_complet, c.Id_Card, c.Denumire, c.Companie;
GO

DROP VIEW View_Card_actual;

----- Adaug view-urile in tabel -----

INSERT INTO Views VALUES
	('View_Card'),
	('View_Bauturi'),
	('View_Card_actual');
GO

DELETE FROM Views;

SELECT * FROM Views;

----- Adaug testele de efectuat in tabela Tests -----

INSERT INTO Tests VALUES
	('DIV_CARD_10'),
	('DIV_BAUTURI_7'),
	('DIV_Card_actual_10')
GO

SELECT * FROM Tests;

----- Fac legatura intre teste si tabele -----

INSERT INTO TestTables VALUES
	-- DIV_CARD_10
	(1, 1, 10, 1),
	-- DIV_BAUTURI_7
	(2, 2, 7, 2),
	-- DIV_Card_actual_10
	(3, 1, 10, 3),
	(3, 3, 10, 4);

SELECT * FROM TestTables;
GO

----- Fac legatura intre teste si view-uri -----

INSERT INTO TestViews VALUES
	(1, 1),
	(2, 2),
	(3, 3);

SELECT * FROM TestViews;


---- Creez procedurile de inserare -----

-- pentru Card
GO
CREATE PROCEDURE ins_test_Card
@NoOfRows INT
AS
BEGIN
	SET NOCOUNT ON;

	DECLARE @denumire NVARCHAR(100);
	DECLARE @companie NVARCHAR(100);
	DECLARE @cod_plata varchar(16);
	DECLARE @tip nvarchar(100);
	DECLARE @data_De_Expirare DATE;
	DECLARE @n INT = 0;
	DECLARE @last_id INT = 
		(SELECT MAX(Card_.Id_Card) FROM Card_)

	WHILE @n < @NoOfRows
	BEGIN
		SET @denumire = 'Card_ TEST ' + CONVERT(VARCHAR(10), @last_id);
		SET @companie = 'CompanieTest';
		SET @cod_plata = '1111222233334444';
		SET @tip = 'Test';
		SET @data_De_Expirare = '2022-11-28';
		INSERT INTO Card_(Companie, Denumire, Cod_plata, Tip, Data_De_Expirare)
		VALUES (@companie, @denumire, @cod_plata, @tip, @data_De_Expirare);
		SET @last_id = @last_id + 1
		SET @n = @n + 1;
	END

	PRINT 'S-au inserat ' + CONVERT(VARCHAR(10), @NoOfRows) + ' valori in Card_.';
END

-- pentru Card
GO
CREATE PROCEDURE ins_test_Bauturi
@NoOfRows INT
AS
BEGIN
	SET NOCOUNT ON;

	DECLARE @fk INT = (Select TOP 1 Id_Meniu from Meniu);
	DECLARE @denumire nvarchar(100);
	DECLARE @descriere nvarchar(100);
	DECLARE @n INT = 0;
	DECLARE @last_id INT = 
		(SELECT MAX(Bauturi.Id_Bautura) FROM Bauturi)

	WHILE @n < @NoOfRows
	BEGIN
		SET @denumire = 'Bautura TEST ' + CONVERT(VARCHAR(10), @last_id);
		SET @descriere = 'descriere' + CONVERT(VARCHAR(10), @last_id);
		INSERT INTO Bauturi(Id_Meniu, Denumire, Descriere)
		VALUES (@fk, @denumire, @descriere);
		SET @last_id = @last_id + 1
		SET @n = @n + 1;
	END

	PRINT 'S-au inserat ' + CONVERT(VARCHAR(10), @NoOfRows) + ' valori in Bauturi.';
END

SELECT * FROM Bauturi;

DROP PROCEDURE ins_test_Bauturi

-- pentru Card_actual
GO
CREATE PROCEDURE ins_test_Card_actual
@NoOfRows INT
AS
BEGIN
	SET NOCOUNT ON;

	DECLARE @n INT = 0;
	DECLARE @Id_Client INT = (Select TOP 1 Client.Id_Client from Client);  -- acest client nu are legaturi
	DECLARE @Id_Card INT;

	DECLARE cursorCard CURSOR FAST_FORWARD FOR
		SELECT Card_.Id_Card FROM Card_ WHERE Card_.Denumire LIKE 'Card_ TEST %';

	OPEN cursorCard;

	FETCH NEXT FROM cursorCard INTO @Id_Card;
	WHILE (@n < @NoOfRows) AND (@@FETCH_STATUS = 0)
	BEGIN
		
		INSERT INTO Card_actual(Id_Client, Id_Card) VALUES (@Id_Client, @Id_Card);
		SET @n = @n + 1;
		
		FETCH NEXT FROM cursorCard INTO @Id_Card;
	END

	CLOSE cursorCard;
	DEALLOCATE cursorCard;

	PRINT 'S-au inserat ' + CONVERT(VARCHAR(10), @n) + ' valori in Card_actual.';
END

SELECT * FROM Client;
SELECT * FROM Card_Actual;

DROP PROCEDURE ins_test_Card_actual

----- Creez procedurile de stergere -----

-- pentru Card
GO
CREATE PROCEDURE del_test_Card
AS
BEGIN
	SET NOCOUNT ON;
	DELETE FROM Card_
	WHERE Card_.Denumire LIKE 'Card_ TEST %';
	PRINT 'S-au sters ' + CONVERT(VARCHAR(10), @@ROWCOUNT) + ' valori din Card_.';
END


-- pentru Bauturi
GO
CREATE PROCEDURE del_test_Bauturi
AS
BEGIN
	SET NOCOUNT ON;
	DELETE FROM Bauturi
	WHERE Bauturi.Denumire LIKE 'Bautura TEST %';
	PRINT 'S-au sters ' + CONVERT(VARCHAR(10), @@ROWCOUNT) + ' valori din Bauturi.';
END


-- pentru Card_actual
GO
CREATE PROCEDURE del_test_Card_actual
AS
BEGIN
	SET NOCOUNT ON;
	DELETE FROM Card_actual;
	PRINT 'S-au sters ' + CONVERT(VARCHAR(10), @@ROWCOUNT) + ' valori din Card_actual.';
END


----- Creez procedura generala de inserare -----

GO
CREATE PROCEDURE inserare_testgen
@idTest INT
AS
BEGIN
	DECLARE @numeTest NVARCHAR(50) = (SELECT T.Name FROM Tests T WHERE T.TestID = @idTest);
	DECLARE @numeTabela NVARCHAR(50);
	DECLARE @NoOfRows INT;
	DECLARE @procedura NVARCHAR(50);

	DECLARE cursorTab CURSOR FORWARD_ONLY FOR
		SELECT Tab.Name, Test.NoOfRows FROM TestTables Test
		INNER JOIN Tables Tab ON Test.TableID = Tab.TableID
		WHERE Test.TestID = @idTest
		ORDER BY Test.Position;
	OPEN cursorTab;

	FETCH NEXT FROM cursorTab INTO @numeTabela, @NoOfRows;
	WHILE (@numeTest NOT LIKE N'DIV_' + @numeTabela + N'_' + CONVERT(NVARCHAR(10), @NoOfRows)) AND (@@FETCH_STATUS = 0)
	BEGIN
		SET @procedura = N'ins_test_' + @numeTabela;
		EXECUTE @procedura @NoOfRows;
		FETCH NEXT FROM cursorTab INTO @numeTabela, @NoOfRows;
	END

	SET @procedura = N'ins_test_' + @numeTabela;
	EXECUTE @procedura @NoOfRows;

	CLOSE cursorTab;
	DEALLOCATE cursorTab;
END

EXECUTE inserare_testgen 1;


----- Creez procedura generala de stergere -----

GO
CREATE PROCEDURE stergere_testgen
@idTest INT
AS
BEGIN
	DECLARE @numeTest NVARCHAR(50) = (SELECT T.Name FROM Tests T WHERE T.TestID = @idTest);
	DECLARE @numeTabela NVARCHAR(50);
	DECLARE @NoOfRows INT;
	DECLARE @procedura NVARCHAR(50);

	DECLARE cursorTab CURSOR FORWARD_ONLY FOR
		SELECT Tab.Name, Test.NoOfRows FROM TestTables Test
		INNER JOIN Tables Tab ON Test.TableID = Tab.TableID
		WHERE Test.TestID = @idTest
		ORDER BY Test.Position DESC;
	OPEN cursorTab;

	FETCH NEXT FROM cursorTab INTO @numeTabela, @NoOfRows;
	WHILE (@numeTest NOT LIKE N'DIV_' + @numeTabela + N'_' + CONVERT(NVARCHAR(10), @NoOfRows)) AND (@@FETCH_STATUS = 0)
	BEGIN
		SET @procedura = N'del_test_' + @numeTabela;
		EXECUTE @procedura;
		FETCH NEXT FROM cursorTab INTO @numeTabela, @NoOfRows;
	END

	SET @procedura = N'del_test_' + @numeTabela;
	EXECUTE @procedura;

	CLOSE cursorTab;
	DEALLOCATE cursorTab;
END

EXECUTE stergere_testgen 1;
SELECT * FROM Card_;


----- Creez procedura generala pentru view-uri -----

GO
CREATE PROCEDURE view_testgen
@idTest INT
AS
BEGIN
	DECLARE @viewName NVARCHAR(50) = 
		(SELECT V.Name FROM Views V
		INNER JOIN TestViews TV ON TV.ViewID = V.ViewID
		WHERE TV.TestID = @idTest);

	DECLARE @comanda NVARCHAR(55) = 
		N'SELECT * FROM ' + @viewName;
	
	EXECUTE (@comanda);
END

EXECUTE view_testgen 1;

----- Creez procedura de rulare a unui test -----

GO
CREATE PROCEDURE run_test
@idTest INT
AS
BEGIN
	DECLARE @startTime DATETIME;
	DECLARE @interTime DATETIME;
	DECLARE @endTime DATETIME;

	SET @startTime = GETDATE();
	
	EXECUTE stergere_testgen @idTest;
	EXECUTE inserare_testgen @idTest;
	
	SET @interTime = GETDATE();
	
	EXECUTE view_testgen @idTest;

	SET @endTime = GETDATE();

	-- var pt insert
	DECLARE @testName NVARCHAR(50) =
		(SELECT T.Name FROM Tests T WHERE T.TestID = @idTest);
	INSERT INTO TestRuns VALUES (@testName, @startTime, @endTime);

	DECLARE @viewID INT =
		(SELECT V.ViewID FROM Views V
		INNER JOIN TestViews TV ON TV.ViewID = V.ViewID
		WHERE TV.TestID = @idTest);
	DECLARE @tableID INT =
		(SELECT TB.TableID FROM Tests T
		INNER JOIN TestTables TT ON T.TestID = TT.TestID
		INNER JOIN Tables TB ON TB.TableID = TT.TableID
		WHERE T.TestID = @idTest AND 
		T.Name LIKE N'DIV_' + TB.Name + N'_' + CONVERT(NVARCHAR(10), TT.NoOfRows));
	DECLARE @testRunID INT = 
		(SELECT TOP 1 T.TestRunID FROM TestRuns T
		WHERE T.Description = @testName
		ORDER BY T.TestRunID DESC);
	
	INSERT INTO TestRunTables VALUES (@testRunID, @tableID, @startTime, @interTime);
	INSERT INTO TestRunViews VALUES (@testRunID, @viewID, @interTime, @endTime);

	PRINT CHAR(10) + '---> TEST COMPLETAT CU SUCCES IN ' + 
		 CONVERT(VARCHAR(10), DATEDIFF(millisecond, @startTime, @endTime)) +
		 ' milisecunde. <---'
END


EXECUTE run_test 3;
GO

SELECT * FROM Card_;
SELECT * FROM Bauturi;
SELECT * FROM Card_Actual;
SELECT * FROM Client;

SELECT * FROM Tables;
SELECT * FROM TestRuns;
SELECT * FROM TestRunTables;
SELECT * FROM TestRunViews;
SELECT * FROM Tests;
SELECT * FROM TestTables;
SELECT * FROM TestViews;
SELECT * FROM Views;



DELETE FROM TestRuns;