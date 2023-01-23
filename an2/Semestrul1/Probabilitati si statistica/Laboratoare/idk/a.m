function out=a()
  N=500;
  S=0;
  for i = 1:N,
    v=zeros(1,366);
    gasit=false;
    for j = 1:23,
      x = randi(365) + 1;
      if v(x) ~= 0,
        gasit=true;
        break;
      endif

      v(x)=1;
    endfor

    if gasit == true,
      S++;
    endif

  endfor

  out=S/N;
end
