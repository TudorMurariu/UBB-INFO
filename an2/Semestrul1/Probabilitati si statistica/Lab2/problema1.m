function out=problema1(N)
  k = 23;
  count=0;
  for i=1:N
    v = randi(365, k, 1)
    if length(unique(v)) < length(v)
      count++;
    endif
  endfor

  out= count/N;
end

