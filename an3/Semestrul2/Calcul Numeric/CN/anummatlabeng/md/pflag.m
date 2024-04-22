function z=pflag(xn,xi)
%pflag(xn,xi) calculeaza polinoamele fundamentale Lagrange
%pentru nodurile xn in punctele xi
%apel z=pflag(xn,xi
np=length(xi);
nn=length(xn);
z=ones(np,nn);
for i=1:nn
   for j=1:nn
      if i~=j
         z(:,i)=z(:,i).*(xi'-xn(j))/(xn(i)-xn(j));
      end
   end
end
