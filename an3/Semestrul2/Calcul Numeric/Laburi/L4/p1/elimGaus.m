
% Eliminare gaussiana cu pivotare scalata pe coloana

function x = elimGaus(A,b)
% eliminare gaussiana cu pivotare scalata pe coloana
% A - matricea sistemului
% b - vectorul termenilor liberi
% x - rezultatul sistemului
  [n, ~] = size(A);
  x = zeros(size(b));
  lineSum = sum(abs(A),2); %calculam suma pe linii
  A = [A,b];
  pivot = 1:n;

  for i=1:n-1
      [u,p] = max(abs(A(i:n,i))./lineSum(i:n));
      if u==0
          error('Solutia nu este unica!')
      end
      p = p+i-1;
      if p~=i
          pivot([i,p]) = pivot([p,i]);
      end
      % determinare valori noi pe liniile de sub pivot
      for j=i+1:n
          m = A(pivot(j),i)/A(pivot(i),i);
          A(pivot(j),i+1:n+1) = A(pivot(j),i+1:n+1)-m*A(pivot(i),i+1:n+1);
      end
  end
  % determinare solutii
  x(n) = A(pivot(n),n+1)/A(pivot(n),n);
  for i=n-1:-1:1
      x(i)=(A(pivot(i),n+1)-A(pivot(i),i+1:n)*x(i+1:n))/A(pivot(i),i);
  end
end
