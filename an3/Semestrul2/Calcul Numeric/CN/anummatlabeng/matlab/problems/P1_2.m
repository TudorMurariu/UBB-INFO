function [S,it]=P1_2(x,err,nmax)
%P1_2 logaritm natural cu dezvoltare MacLaurin
%apel [S,it]=P1_2(x,err,nmax)
%x argumentul (in [-1,1])
%err - eroarea (implicit eps)
%S - valoarea aproximativa a logaritmului
%it - numar de iteratii necesare

if nargin<3, nmax=2000; end
if nargin<2, err=eps; end
if x==0
    S=0;
    it=0;
    return
end
    
px=x; t=px; 
S=px;
for k=2:nmax
    px=-px*x; t=px/k;
    S=S+t;
    if abs(t)< err*abs(S)
        it=k;
        return
    end
end
error('prea multe iteratii')