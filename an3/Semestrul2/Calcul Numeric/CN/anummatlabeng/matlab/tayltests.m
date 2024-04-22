x=0.25; n=1:6; 
c=1./cumprod([1 n]);
for k=1:7, T(k)=polyval(c(k:-1:1),x); end
X=[(0:6)',T', abs(T-exp(x))'];
fprintf('\n n |      T_n(x)     | |T_n(x)-exp(x)|\n');
fprintf('--------------------------------------\n');
fprintf(' %d | %15.12f |    %8.3e\n', X' )
fprintf('--------------------------------------\n');