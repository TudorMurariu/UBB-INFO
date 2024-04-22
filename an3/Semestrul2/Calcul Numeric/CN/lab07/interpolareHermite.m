function [ res ] = interpolareHermite( x, f, fd, point )
% aproximeaza x folosind interpolarea Hermite

% initializare
[rows, m] = size(x);
q = zeros(2 * m + 2, 2 * m + 2);
z = zeros(2 * m + 2);

% z contine x0 x0 x1 x1 x2 x2 ...
% Q contine f(x0) f(x0) f(x1) x(x1) ...
%           - f'(x0) f[x1,x0] f'(x1) f[x2,x1] ...
for i = 1 : m
    % p2
    z(2 * i - 1) = x(i);
    z(2 * i) = x(i);
    q(2 * i - 1, 1) = f(i);
    q(2 * i, 1) = f(i);
    q(2 * i, 2) = fd(i);
    % p3
    if i ~= 1
        q(2*i-1,2)=(q(2*i-1,1)-q(2*i-2,1))/(z(2*i-1)-z(2*i-2));
    end
end

% se completeaza diferentele divizate
for i = 3 : 2 * m
   for j = 3 : i
      q(i,j)=(q(i,j-1)-q(i-1,j-1))/(z(i)-z(i-j+1)); 
   end
end

% calc p(i)
s = 1;
p = q(1, 1);
for i = 2 : 2 * m
   a = abs(x(floor(i/2)) - point);
   s = s * a;
   prev = p;
   p = p + s * q(i,i);
   res = p;
   if prev-p<1e-5
      break 
   end
end

endfunction