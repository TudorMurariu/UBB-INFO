function c = barycentricweigths( x )
%BARYCENTRICWEIGHTS - compute barycentric weights(coefficient)   
%call c = barycentricweigths( x )
%x - nodes
%c - weights

n=length(x)-1;
c=ones(1,n+1);
for j=1:n+1
    c(j)=prod(x(j)-x([1:j-1,j+1:n+1]));
end
c=1./c;
end

