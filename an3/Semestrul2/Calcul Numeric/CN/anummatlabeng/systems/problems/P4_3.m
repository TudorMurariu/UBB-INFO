function P4_3(A)
%determina descompunerea LUP a matricei A si determinatul
%apel [L,U,P,D]=P4_3(A)

[L,U,P,D]=lupdet(A);
D
det(A)