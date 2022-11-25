function out=p2a(k=10, p=0.5)
  #
  pos = zeros(1,k+1);
  for i=2:k+1
    pos(i) = pos(i-1) + 2 * binornd(1,p) - 1;
  endfor
  out=pos;
end
