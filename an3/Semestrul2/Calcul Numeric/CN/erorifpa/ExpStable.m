function s=ExpStable(x)
% EXPSTABLE stable computation of the exponential function
% s=Exp(x); computes an approximation s of exp(x) up to machine
% precision.
if x<0 
    v=-1; x=abs(x);
else
    v=1;
end
so=0; s=1; term=1; k=1;
while s~=so
    so=s; term=term*x/k;
    s=so+term; k=k+1;
end
if v<0, s=1/s; end
end

