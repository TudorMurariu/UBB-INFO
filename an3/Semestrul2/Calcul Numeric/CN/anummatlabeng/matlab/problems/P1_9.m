function A=P1_9(n)
%P1_9 - triunghiul lui Pascal

A=zeros(n,n+1);
A(:,1)=ones(n,1); A(1,2)=1;
for i=2:n
    for j=2:i+1
        A(i,j)=A(i-1,j-1)+A(i-1,j);
    end
end