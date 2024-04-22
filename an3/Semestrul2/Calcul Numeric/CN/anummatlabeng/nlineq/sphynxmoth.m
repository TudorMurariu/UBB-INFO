function [c,cc,er1,er2]=sphynxmoth
load sphynx
[w,ii]=sort(sphynx(:,1)); 
r=sphynx(ii,2);
lW=log(w);
lr=log(r);
A=[lW,ones(length(w),1)];
c=A\lr;
vr1=exp(c(2))*w.^c(1);
er1=r-vr1;
plot(w,r,'o',w,vr1);
A=[lW.^2,A];
cc=A\lr;
figure(2)
vr2=polyval(cc,lW);
plot(lW,lr,'o',lW,vr2)
evr2=exp(vr2);
er2=r-evr2;
figure(3)
plot(w,r,'o',w,evr2);