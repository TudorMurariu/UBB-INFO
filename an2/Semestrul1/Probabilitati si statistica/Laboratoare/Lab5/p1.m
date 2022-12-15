function v=p1(x,p,n)
  v = [];
  for i=1:n
    fx = rand;
    interval = x(1);
    s = 0;
    for j=1:length(p)

      if fx > s
        interval = x(j);
      endif
      s = s + p(j);
    endfor
    v = [v,interval];
  endfor
end
