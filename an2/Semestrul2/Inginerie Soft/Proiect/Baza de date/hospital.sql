create table users(
	id int primary key generated always as identity,
	username varchar(100),
	password varchar(100),
	type varchar(100)
);

create table drugs(
	id int primary key generated always as identity,
	name varchar(100),
	price float,
	description varchar(100)
);

create table orders(
	id int primary key generated always as identity,
	quantity int,
	status varchar(100),
	userid int
);

alter table orders
add foreign key (userid) references users(id) on delete cascade on update cascade;

create table orderitems(
	drugid int,
	orderid int,
	quantity int,
	drugName varchar(100)
);

alter table orderitems
add constraint pk_orderitems primary key (drugid, orderid);

alter table orderitems 
add constraint fk_drug foreign key(drugid) references drugs(id) on delete cascade on update cascade;

alter table orderitems 
add constraint fk_order foreign key(orderid) references orders(id) on delete cascade on update cascade;



