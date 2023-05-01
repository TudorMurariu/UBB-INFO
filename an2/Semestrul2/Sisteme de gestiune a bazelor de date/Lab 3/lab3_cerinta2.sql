create or alter function dbo.validare_Text (@denumire varchar(50))
returns bit
as
begin 
	declare @flag bit
	set @flag = 1

	if @denumire is null or @denumire =''
		set @flag=0;
	return @flag;
end
go

create or alter function dbo.validare_Cod_plata (@denumire varchar(50))
returns bit
as
begin 
	declare @flag bit
	set @flag = 1

	if @denumire is null or @denumire ='' or LEN(@denumire) <> 16
		set @flag=0;
	return @flag;
end
go


CREATE OR ALTER PROCEDURE AddClient
	@nume_complet varchar(100), @email varchar(100), @parola varchar(100), @Id_Adresa_actuala int, @Id_Adresa int as
begin 
begin tran

	begin try
		--- validari texte
		if dbo.validare_Text(@nume_complet) <> 1
			begin 
				print 'Nume Complet'
				raiserror('Nume Complet invalid',14,1);
			end
		if dbo.validare_Text(@email) <> 1
			begin 
				print 'email'
				raiserror('Email invalid',14,1);
			end
		if dbo.validare_Text(@parola) <> 1
			begin 
				print 'parola'
				raiserror('Parola invalida',14,1);
			end

		INSERT INTO Client(nume_complet, email, parola, Id_Adresa_actuala, Id_Adresa)
		values 
		(@nume_complet, @email, @parola, @Id_Adresa_actuala, @Id_Adresa);

		print 'Client adaugat'

		commit tran
		print 'Transaction committed'
	end try

	begin catch
		rollback tran
		print ERROR_MESSAGE(); 
		print 'Transaction rollbacked'
		return 0
	end catch

	return 1
end

go
CREATE OR ALTER PROCEDURE AddCard
	@Companie varchar(100), @Denumire varchar(100), @Cod_plata varchar(100), @Tip varchar(100), @Data_De_Expirare date as
begin 
begin tran
	begin try

		if dbo.validare_Text(@Companie) <> 1
			begin 
				print 'companie'
				raiserror('Companie invalida',14,1);
			end
		if dbo.validare_Text(@Denumire) <> 1
			begin 
				print 'denumire'
				raiserror('Denumire invalida',14,1);
			end
		if dbo.validare_Text(@Tip) <> 1
			begin 
				print 'tip'
				raiserror('Tip invalid',14,1);
			end

		--- validare cod plata 
		if dbo.validare_Cod_plata(@Cod_plata) <> 1
			begin 
				print 'Cod plata'
				raiserror('Cod plata invalid',14,1);
			end

		INSERT INTO Card_(Companie, Denumire, Cod_plata, Tip, Data_De_Expirare)
		values 
		(@Companie, @Denumire, @Cod_plata, @Tip, @Data_De_Expirare);

		print 'Card adaugat'

		commit tran
		print 'Transaction committed'
	end try

	begin catch
		rollback tran
		print ERROR_MESSAGE(); 
		print 'Transaction rollbacked'
		return 0
	end catch

	return 1
end

go
CREATE OR ALTER PROCEDURE AddClientCard_2 
	@nume_complet varchar(100), @email varchar(100), @parola varchar(100), @Id_Adresa_actuala int, @Id_Adresa int,
	@Companie varchar(100), @Denumire varchar(100), @Cod_plata varchar(100), @Tip varchar(100), @Data_De_Expirare date as
begin 
		
		declare @client_added int;
		declare @card_added int;

		EXEC @client_added = AddClient @nume_complet, @email, @parola, @Id_Adresa_actuala, @Id_Adresa
		EXECUTE @card_added = AddCard @Companie, @Denumire, @Cod_plata, @Tip, @Data_De_Expirare;
		
		if @client_added <> 1
			begin
				print 'client was not added so we can not add to actual_card'
				return 0
			end

		if @card_added <> 1
			begin
			print 'card was not added so we can not add to actual_card'
				return 0
			end

		declare @Id_Client int;
		declare @Id_Card int;

		SELECT TOP 1 @Id_Client = C.Id_Client
		FROM dbo.Client as C
		WHERE C.nume_complet = @nume_complet;

		SELECT TOP 1 @Id_Card = C.Id_Card
		FROM dbo.Card_ as C
		WHERE C.Cod_plata = @Cod_plata;

		INSERT INTO Card_Actual
		values 
		(@Id_Client, @Id_Card);

		print 'card_actual adaugat'
end


Select * from Client;
Select * from Card_;
Select * from Card_Actual;
EXECUTE AddClientCard_2 'Marian3 David3333','marian.david@gmail.com', 'parola123', 12, 12,
	'Revolut', 'Marian2 David', '7775199181819392', 'Credit', '2028-10-20';
Select * from Client;
Select * from Card_;
Select * from Card_Actual;

Select * from Client;
Select * from Card_;
Select * from Card_Actual;
EXECUTE AddClientCard_2 '','marian.david@gmail.com', 'parola123', 12, 12,
	'Revolut', 'Marian David', '7775199181819392', 'Credit', '2028-10-20';
Select * from Client;
Select * from Card_;
Select * from Card_Actual;

Select * from Client;
Select * from Card_;
Select * from Card_Actual;
EXECUTE AddClientCard_2 'Marian3 David','marian.david@gmail.com', 'parola123', 12, 12,
	'Revolut', 'Marian David', '55555', 'Credit', '2028-10-20';
Select * from Client;
Select * from Card_;
Select * from Card_Actual;