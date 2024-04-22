function ro=encode(c,s)
if c==0
    ro=1;
elseif abs(s)<abs(c)
    ro=sign(c)*s/2;
else
    ro=2*sign(s)/c;
end