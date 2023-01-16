p(1).
p(2).
miau(1).
miau(2).
r(1).
r(2).

ham:- p(X), !,
     miau(Y), 
     r(Z),
     write(X),
     write(Y),
     write(Z), nl.