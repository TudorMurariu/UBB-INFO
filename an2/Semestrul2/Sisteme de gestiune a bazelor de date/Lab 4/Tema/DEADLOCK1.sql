SET DEADLOCK_PRIORITY HIGH
-- SET DEADLOCK_PRIORITY LOW

BEGIN TRANSACTION
	update Tip_Vehicul set Denumire = 'Transaction 2'
	where Tonaj = 2;

	WAITFOR DELAY '00:00:07';

	update Meniu set Url_site = 'Transaction 2'
	where Id_Meniu = 13;
COMMIT TRAN;

select * from Meniu;
select * from Tip_Vehicul;

update Meniu set Url_site = 'AAAABBBBBBBCCCCCCCCCCCC'
	where Id_Meniu = 13;
update Tip_Vehicul set Denumire = '4x4'
	where Tonaj = 2;