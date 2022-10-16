function out=aranjamentele(v,n,k)
  A = nchoosek(v([1:n]),k);
  [l,c] = size(A);
  for i=1:1:l
    disp(perms(A(i,:)))
  endfor
  %disp(nchoosek(flip(v([1:n])),k))
end
