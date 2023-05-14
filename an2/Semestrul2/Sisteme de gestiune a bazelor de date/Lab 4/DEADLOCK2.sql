-- SET DEADLOCK_PRIORITY LOW
-- SET DEADLOCK_PRIORITY HIGH

BEGIN TRANSACTION
	update Meniu set Url_site = 'Transaction 1'
	where Id_Meniu = 13;

	WAITFOR DELAY '00:00:07'

	update Tip_Vehicul set Denumire = 'Transaction 1'
	where Tonaj = 2;
COMMIT TRAN;

select * from Meniu;


update Meniu set Url_site = 'AAAABBBBBBBCCCCCCCCCCCC'
	where Id_Meniu = 13;
update Tip_Vehicul set Denumire = '4x4'
	where Tonaj = 2;