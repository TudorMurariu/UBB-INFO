%problema 10.1

ec1=@(x,y) 1-y^2;
se=@(x) (exp(2*x)-1)./(exp(2*x)+1);
[t1,w1]=ode23(ec1,[0,1],0);
[t2,w2]=ode45(ec1,[0,1],0);
se1=se(t1);
se2=se(t2);
d1=abs(w1-se1);
d2=abs(w2-se2);
[h1,i1]=sort(diff(t1));
[h2,i2]=sort(diff(t2));
plot(t1,w1,t1,se1,t2,w2);
figure(2)
semilogy(h1,d1(i1+1),h1,h1.^2,h2,d2(i2+1),h2,h2.^4)