function P3_9
%P3_9 - formula lui Heron pentru aria triunghiului
%vezi D. Goldberg - What every computer scientist
%should know abou floating-point arithmetic
%ACM Computer Surveys, 23(1), 1991, pp. 5-48

%exemplu fara anulare
format long
disp('fara anulare')
a=5, b=4, c=8
S1=Heron1(a,b,c)
S2=Heron2(a,b,c)
abs(S1-S2)

disp('cu anulare')
format hex, disp('hexa:')
a=10, b=5+20*eps,  c=5+20*eps,
format long, disp('double:')
a,b,c
S1=Heron1(a,b,c)
S2=Heron2(a,b,c)
abs(S1-S2)

function S=Heron1(a,b,c)
%HERON1 - aplicare directa, posibila anulare
p=(a+b+c)/2;
S=sqrt(p*(p-a)*(p-b)*(p-c));

function S=Heron2(a,b,c)
%HERON2 - evitarea anularii
ll=sort([a,b,c]);
a=ll(3); b=ll(2); c=ll(1);
S=((a+(b+c))*(c-(a-b))*(c+(a-b))*(a+(b-c)))^(1/2)/4;
