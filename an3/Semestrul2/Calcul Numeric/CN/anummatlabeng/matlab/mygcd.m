function d=mygcd(a,b)
%MYGCD - recursive greatest common divisor
%   D = MYGCD(A,B) computes the greatest
%       common divisor of A and B

if a==0 & b==0, then d = NaN; 
elseif b==0
    d = a;
else
    d = mygcd(b, mod(a,b));
end