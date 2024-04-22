%P3_3
%conditionarea radacinilor unui polinom

%exemplul lui Wilkinson
%p=(x-1)(x-2)...(x-n), n=20
xi=1:20;
p1=poly(xi);
nc=condpol(p1,xi);
[nc,ii]=sort(nc);
format short g
[ii',nc']
%studiu grafic perturbatie normala
subplot(1,2,1)
wilkinson(20)
axis equal
title('perturbatie normala')
%studiu grafic perturbatie uniforma
subplot(1,2,2)
wilkinsonu(20)
axis equal
title('perturbatie uniforma')

%exemplul 2
%a(i)=2^(-k), k=0..n, n=20
p2=[1,2.^(-[1:20])];
nc2=condpol(p2);
[nc2,ii]=sort(nc2);
[ii,nc2]
format short 
