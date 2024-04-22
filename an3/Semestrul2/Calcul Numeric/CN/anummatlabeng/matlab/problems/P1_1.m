%P1_1 - serie armonica generalizata
k=1:200; k2=20:200;
S=cumsum(1./k.^2);
S=S(k2);
err=S-pi^2/6*ones(1,length(k2))