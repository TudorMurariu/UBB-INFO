function q=deCasteljau(pc,t)
%algoritmul de Casteljau
%apel q=deCasteljau(pc,t)
%pc - punctele de control
%t punctele in care se evalueaza
[mp,k]=size(pc);
lt=length(t);
for l=1:lt
    qn=pc;
    for r=1:k
        i=r+1:k;
        qn(:,i)=(1-t(l))*qn(:,i-1)+t(l)*qn(:,i);
    end
    q(:,l)=qn(:,k);
end;
