INSERT INTO Restaurant(Nume_Restaurant, Id_Adresa, Id_Meniu) VALUES
('Moldavian', 11, 7);

BEGIN TRANSACTION
	WAITFOR DELAY '00:00:07'
	UPDATE Restaurant SET Nume_Restaurant = 'Moldavian123'
	WHERE  Nume_Restaurant = 'Moldavian'
COMMIT TRAN;

select * from Restaurant;
delete from Restaurant where Nume_Restaurant = 'Moldavian';
delete from Restaurant where Nume_Restaurant = 'Moldavian123';
