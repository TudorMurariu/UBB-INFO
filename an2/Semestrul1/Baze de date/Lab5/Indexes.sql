--- Validare ID adrersa ---
CREATE OR ALTER Function IS_Valid_Adress(@Id_Adresa INT)
   RETURNS INT
AS
BEGIN
	IF EXISTS (SELECT * from Adresa where @Id_Adresa = Adresa.Id_Adresa)
	BEGIN
		RETURN 1
	END

	RETURN 0
	 
END
GO

--- NOT NULL ---

CREATE OR ALTER Function IS_NOT_NULL(@string nvarchar(100))
   RETURNS INT
AS
BEGIN
	IF @string IS NOT NULL
	BEGIN
		RETURN 1
	END

	RETURN 0
	 
END
GO

----- CRUD PENTRU CLIENT -----
GO
CREATE OR ALTER PROCEDURE CRUD_CLIENT
    @nume_complet nvarchar(100),
	@email  nvarchar(100),
	@parola nvarchar(100),
	@Id_Ardesa_actuala INT,
	@Id_Adresa INT,
    @num_of_rows INT = 1
AS
BEGIN
    SET NOCOUNT ON;
    IF (dbo.IS_NOT_NULL(@nume_complet) = 1 AND
        dbo.IS_NOT_NULL(@email) = 1 AND
        dbo.IS_NOT_NULL(@parola) = 1 AND
		dbo.IS_Valid_Adress(@Id_Ardesa_actuala) = 1 AND
		dbo.IS_Valid_Adress(@Id_Adresa) = 1)
    BEGIN
        -- INSERT
        DECLARE @n INT = 0;
        WHILE (@n < @num_of_rows)
        BEGIN
            INSERT INTO CLIENT VALUES(@nume_complet, @email, @parola, @Id_Ardesa_actuala, @Id_Adresa);
            SET @n = @n + 1;
        END

        -- SELECT
        SELECT * FROM CLIENT ORDER BY CLIENT.nume_complet;

        -- UPDATE
        UPDATE CLIENT
        SET CLIENT.nume_complet = @nume_complet + '_CRUD'
        WHERE CLIENT.nume_complet = @nume_complet;

        SELECT * FROM CLIENT ORDER BY CLIENT.nume_complet;
        
        -- DELETE

        DELETE FROM CLIENT
        WHERE CLIENT.nume_complet LIKE @nume_complet + '_CRUD';

        SELECT * FROM CLIENT ORDER BY nume_complet;

        PRINT 'Operatiile CRUD pentru CLIENT au fost executate.';
    END
    ELSE
    BEGIN
        RAISERROR('Datele de intrare nu sunt valide.', 16, 1);
    END
END
GO

SELECT * FROM CLIENT;
EXEC CRUD_CLIENT 'TEST', 'email@yahoo.com', 'parola123', 10, 10, 3;

--- Validare Lungime @Cod_plata ---
GO
CREATE OR ALTER Function IS_Valid_Cod_Plata (@Cod varchar(16))
   RETURNS INT
AS
BEGIN
	IF (Len(@Cod) = 16)
	BEGIN
		RETURN 1
	END

	RETURN 0
END
GO

----- CRUD PENTRU CARD -----

GO
CREATE OR ALTER PROCEDURE CRUD_CARD
    @Companie nvarchar(100),
	@Denumire nvarchar(100),
	@Cod_plata varchar(16),
	@Tip nvarchar(100),
	@Data_De_Expirare DATE,
    @num_of_rows INT = 1
AS
BEGIN
    SET NOCOUNT ON;
    IF (dbo.IS_NOT_NULL(@Companie) = 1 AND
        dbo.IS_NOT_NULL(@Denumire) = 1 AND
        @Cod_plata IS NOT NULL AND
		dbo.IS_NOT_NULL(@Tip) = 1 AND
		dbo.IS_Valid_Cod_Plata(@Cod_plata) = 1 AND
		@Data_De_Expirare IS NOT NULL)
    BEGIN
        -- INSERT
        DECLARE @n INT = 0;
        WHILE (@n < @num_of_rows)
        BEGIN
            INSERT INTO Card_ VALUES(@Companie, @Denumire, @Cod_plata, @Tip, @Data_De_Expirare);
            SET @n = @n + 1;
        END

        -- SELECT
        SELECT * FROM Card_ ORDER BY Card_.Denumire;

        -- UPDATE
        UPDATE Card_
        SET Card_.Denumire = @Denumire + '_CRUD'
        WHERE Card_.Denumire = @Denumire;

        SELECT * FROM Card_ ORDER BY Card_.Denumire;
        
        -- DELETE

        DELETE FROM Card_
        WHERE Card_.Denumire LIKE @Denumire + '_CRUD';

        SELECT * FROM Card_ ORDER BY Denumire;

        PRINT 'Operatiile CRUD pentru Card au fost executate.';
    END
    ELSE
    BEGIN
        RAISERROR('Datele de intrare nu sunt valide.', 16, 1);
    END
END
GO

SELECT * FROM Card_;
EXEC CRUD_CARD 'CompanieTest', 'cardTest', '3234165487698655', 'Tip Test', '2026-09-13', 3;

--- Validare ID Client ---
GO
CREATE OR ALTER Function IS_Valid_IdClient(@Id_client INT)
   RETURNS INT
AS
BEGIN
	IF EXISTS (SELECT * from Client where @Id_client = Client.Id_Client)
	BEGIN
		RETURN 1
	END

	RETURN 0
	 
END
GO

--- Validare ID Card ---
GO
CREATE OR ALTER Function IS_Valid_IdCard(@Id_Card INT)
   RETURNS INT
AS
BEGIN
	IF EXISTS (SELECT * from Card_ where @Id_Card = Card_.Id_Card)
	BEGIN
		RETURN 1
	END

	RETURN 0
	 
END
GO

----- CRUD PENTRU CARD_ACTUAL -----
GO
CREATE OR ALTER PROCEDURE CRUD_CARD_ACTUAL
    @Id_Client INT,
	@Id_Card INT
AS
BEGIN
    SET NOCOUNT ON;
    IF (dbo.IS_Valid_IdClient(@Id_Client) = 1 AND
		dbo.IS_Valid_IdCard(@Id_Card) = 1)
    BEGIN
        -- INSERT
        INSERT INTO Card_Actual VALUES(@Id_Client, @Id_Card);

        -- SELECT
        SELECT * FROM Card_Actual ORDER BY Card_Actual.Id_Card;

        -- UPDATE
        UPDATE Card_Actual
        SET Card_Actual.Id_Client = @Id_Client + 1
        WHERE Card_Actual.Id_Client = @Id_Client;

        SELECT * FROM Card_Actual ORDER BY Card_Actual.Id_Client;
        
        -- DELETE

        DELETE FROM Card_Actual
        WHERE Card_Actual.Id_Card = @Id_Card AND
			  Card_Actual.Id_Client = @Id_Client + 1;

        SELECT * FROM Card_Actual ORDER BY Card_Actual.Id_Client;

        PRINT 'Operatiile CRUD pentru Card_Actual au fost executate.';
    END
    ELSE
    BEGIN
        RAISERROR('Datele de intrare nu sunt valide.', 16, 1);
    END
END
GO

SELECT * FROM Card_Actual;
EXEC CRUD_CARD_ACTUAL 59, 17;



-------------------- Views ------------------------------------------
GO
CREATE OR ALTER VIEW View_Client 
AS
 SELECT nume_complet, parola
 FROM Client as C
 INNER JOIN Card_Actual as CA
 ON C.Id_Client = CA.Id_Client
 where Len(parola) > 5;
GO



GO
CREATE OR ALTER VIEW Client_Card 
AS
 SELECT C.nume_complet, C.parola, Card_.Companie, Card_.Denumire
 FROM Client as C
 INNER JOIN Card_Actual as CA
 ON C.Id_Client = CA.Id_Client
 INNER JOIN Card_
 ON Card_.Id_Card = CA.Id_Card;
GO

---------- INDEXI --------------------------------------------
-- Client
CREATE NONCLUSTERED INDEX N_idx_Parola ON Client (parola);
CREATE NONCLUSTERED INDEX N_idx_Nume_complet ON Client (nume_complet);
CREATE NONCLUSTERED INDEX N_idx_Id_Client ON Client (Id_Client);

-- Card
CREATE NONCLUSTERED INDEX N_idx_Companie ON Card_ (Companie);
CREATE NONCLUSTERED INDEX N_idx_Denumire ON Card_ (Denumire);
CREATE NONCLUSTERED INDEX N_idx_Id_Card ON Card_ (Id_Card);


-- Card_Actual
CREATE NONCLUSTERED INDEX N_idx_Id_Client ON Card_Actual (Id_Client);
CREATE NONCLUSTERED INDEX N_idx_Id_Card ON Card_Actual (Id_Card);

/*-- Drop
DROP INDEX N_idx_Parola ON Client;
DROP INDEX N_idx_Nume_complet ON Client;
DROP INDEX N_idx_Id_Client ON Client;

DROP INDEX N_idx_Companie ON Card_;
DROP INDEX N_idx_Denumire ON Card_;
DROP INDEX N_idx_Id_Card ON Card_;

DROP INDEX N_idx_Id_Client ON Card_Actual;
DROP INDEX N_idx_Id_Card ON Card_Actual;*/

SELECT * FROM View_Client;
SELECT * FROM Client_Card;