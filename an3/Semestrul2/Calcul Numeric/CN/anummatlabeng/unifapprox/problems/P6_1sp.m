function P6_1sp
%P6_1sp problema 6_1; varianta simpla precizie
format long
for m=3:3:12
    m
    [ec,c,ea]=enBernstein(m)
    pause
end

function [ec,c,ea]=enBernstein(m)
%aproximatie continua Bernstein mcmmp
A=single(zeros(m+1));
b=single(zeros(m+1,1));
for i=0:m
    for j=0:m
        A(i+1,j+1)=nchoosek(m,i)*nchoosek(m,j)*...
            single(beta(i+j+1,2*m-i-j+1));
    end
end
b=single(1/(m+1))*ones(m+1,1);
ec=condest(A);
c=A\b;
t=single(linspace(0,1,100));;
vb=single(zeros(size(t)));
for k=0:m
    vb=vb+c(k+1)*nchoosek(m,k)*t.^k.*(1-t).^(m-k);
end
ea=norm(vb-ones(size(t)),inf);

    
