%P9_2
P=gallery('pascal',12);
F=gallery('frank',12);
eP=sort(eig(P)');
ieP=sort(1./eP);
norm(eP-ieP,inf)

ceP=condeig(P)'
eF=sort(eig(F)');
ieF=sort(1./eF);
norm(eF-ieF,inf)

ceF=condeig(F)'