function H=HermitePoly(x,r,t,f)
syms s
m=numel(x)-1;
s=sym(0);
for k=0:m
    s=s+HermiteBasicPoly2(x,r,k,0,t)*subs(f,t,x(k+1));
end
for k=0:m
    for j=1:r(k+1)
        s=s+HermiteBasicPoly2(x,r,k,j,t)*subs(diff(f,t,j),t,x(k+1));
    end
end
H=s;
end

function hkj=HermiteBasicPoly2(x,r,k,j,t)
syms u nu z
m=length(x);
i=[1:k,k+2:m];
u=prod((t-x(i)).^(r(i)+1));
z=0;
for nu=0:r(k+1)-j
    z=z+(t-x(k+1))^nu/factorial(nu)*subs(diff(1/u,t,nu),t,x(k+1));
end
hkj=(t-x(k+1))^j/factorial(j)*u*z;
end