function [c,s]=Givens(a,b)
%GIVENS - obtine c=cos(t) si s=sin(t) din a,b

if b==0
    c=1; s=0;
else 
    if abs(b)>abs(a)
        t=-a/b; s=1/sqrt(1+t^2); c=s*t;
    else
        t=-b/a; c=1/sqrt(1+t^2); s=c*t;
    end
end