#Test 3 Err problema 5, subpunctul 5
a=10;
y=zeros(1,30);
# y(n)=y_(n-1) <-> indexarea e de la 0
# marginea inferioara pentru y_30
y(30)=1/((a+1)*31);
for n=29:-1:1
  y(n)=(1-(n+1)*y(n+1))/(a*(n+1));
endfor 

# initializam variabilele si matricie pentru integrare

yi = zeros(1, 30);
for n=1:30
  f=@(x)(x.^n)./(x+a);
  yi(n)=quad(f,0,1);
endfor

err=[abs(y-yi)];
format long;
[y', yi', err']