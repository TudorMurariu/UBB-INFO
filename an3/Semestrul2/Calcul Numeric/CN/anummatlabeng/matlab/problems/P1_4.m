function g=P1_4(m,n)
%P1_4 cel mai mare divizor comun
g=cmmdc(m,n);
function g=cmmdc(m,n)
%CMMDC - cel mai mare divizor comun
%apel g=cmmdc(m,n)
%m,n intregi
%pentru o implementare profesionala vezi functia MATLAB GCD

if ~isequal(m,round(m))||~isequal(n,round(n))
    error('argumentele nu sunt intregi');
end
if (m==0) & (n==0), 
    g=0;
    return
end
if mod(m,n)==0
    g=n;
    return
else
    g=cmmdc(n,mod(m,n));
end