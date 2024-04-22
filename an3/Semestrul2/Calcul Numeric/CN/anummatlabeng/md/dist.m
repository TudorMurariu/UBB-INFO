function md=dist(X,P)
%computes the distance matrix -- md(i,j)=dist(P(i),X(j))
[mx,nx]=size(X);
[mp,np]=size(P);
if mx~=mp
    error('X and P must have the same dimensionality')
end
for i=1:np
    for j=1:nx
        md(i,j)=norm(P(:,i)-X(:,j));
    end
end
