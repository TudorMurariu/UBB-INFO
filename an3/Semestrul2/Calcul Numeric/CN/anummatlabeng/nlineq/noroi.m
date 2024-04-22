function [K,p]=noroi
%functii imbricate
x0=[7,0.1,2.1];
x0=fminsearch(@fnoroimin,x0);
[K,nit]=Newton(@fnoroi,@fnoroid,x0,1e-6);
k=K;
r0=50;
p=Newton(@pres,@dpres,r0);
    function y=fnoroi(k)
        y=[k(1)*exp(k(2))+k(3)-10;...
            k(1)*exp(2*k(2))+2*k(3)-12;...
            k(1)*exp(3*k(2))+3*k(3)-15];
    end
    function y=fnoroid(k)
        y=[exp(k(2)), k(1)*exp(k(2)), 1;...
            exp(2*k(2)), 2*k(1)*exp(2*k(2)), 2;...
            exp(3*k(2)), 3*k(1)*exp(3*k(2)), 3];
    end
    function y=fnoroimin(k)
        y=(k(1)*exp(k(2))+k(3)-10)^2+...
            (k(1)*exp(2*k(2))+2*k(3)-12)^2;...
            (k(1)*exp(3*k(2))+3*k(3)-15)^2;
    end
    function y=pres(r)
        y=k(1)*exp(k(2)*r)+k(3)*r-500;
    end
    function y=dpres(r)
        y=k(1)*k(2)*exp(k(2)*r)+k(3);
    end
end