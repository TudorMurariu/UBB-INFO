function x=hex2float(s)
%HEX2FLOAT conversie hexa in flotant
%s - sir hexa(16 cifre)
%x - rezultatul
sg=1;
if s(1)>'7'
    sg=-1;
end
e=sscanf(s(1:3),'%x'); e=mod(e,2048);
e=e-1023;
f=0;
for k=4:16
    f=f+cod(s(k))*16^(13-k+3);
end
f=f/16^13;
x=sg*pow2(f+1,e);
function c=cod(s)
c=sscanf(s,'%x');