function [L, U, P] = lup_hessenberg(A)
  % LUP decomposition of a Hessenburg matrix
  % A - Hessenburg matrix to be decomposed
  % L - matrix of multipliers
  % U - upper triangular matrix
  % P - permutation matrix 
  [m, n] = size(A);
  P = zeros(m, n);
  piv = (1:m)';
  for i=1:m-1
    [maxim, poz] = max(abs([A(i,i), A(i+1, i)])); % (1)
    poz += i - 1;
    if i ~= poz
      A([i,poz],:) = A([poz,i],:);
      piv([i,poz]) = piv([poz,i]);
    endif
    A(i+1,i) = A(i+1,i)/A(i,i); % (2)
    A(i+1,i+1:n) -= A(i+1,i)*A(i,i+1:n); % (3)
  endfor
  for i = 1:m
    P(i,piv(i)) = 1;
  end
  U = triu(A); 
  L = tril(A,-1); 
  L = L + eye(m); 
end
# Complexitatea algoritmului:
# O(n^2), unde n este numarul de linii din matricea A 
# Optimizari:
# (1) - maximul se face doar intre elementul de pe diagonala principala si elementul de sub el
# (2) - se imparte doar elementul de sub pivot
# (3) - operatia nu se mai face la nivel de matrice de dimensiunea (i-1) * (i-1), 
# ci la nivel de vector de dimensiunea (i-1)