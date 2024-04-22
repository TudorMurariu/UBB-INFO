function [x,ni,rez]=P8_6(n,a0,an)
%P8_6
%n -dimensiune
%a0,an - valori date
%x - solutia
%ni - nr. de iteratii
%rez - valoarea functiei in radacina
x0=(1-1/n*[0:n-1])';
[x,ni]=Newton(@fchim,@dfchim,x0,1e-8,0,200);
rez=norm(fchim(x));

    function y=fchim(a)
        n=length(a);
        y=zeros(n,1);
        y(1)=a(n)*a(1)^2+a(1)-a0;
        y(2:n-1)=a(n)*a(2:n-1).^2+a(2:n-1)-a(1:n-2);
        y(n)=a(10)*an^2+an-a(n-1);
    end

    function y=dfchim(a)
        n=length(a);
        y=zeros(n);
        y=diag([2*a(1:n-1)*a(n)+1;an^2])-diag(ones(n-1,1),-1);
        y(:,n)=[a(1:n-1).^2;an^2];
    end
end