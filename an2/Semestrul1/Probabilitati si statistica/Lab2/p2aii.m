function out=p2aii(n)
  clf;
  hold on;
  axis square;
  # uhh

  rectangle('Position', [0 0 1 1]);
  count = 0;
  for i=1:n
    x = rand;
    y = rand;
    if pdist([x y;0.5 0.5]) < pdist([x y;0 0]) && pdist([x y;0.5 0.5]) < pdist([x y;0 1]) && pdist([x y;0.5 0.5]) < pdist([x y;1 0]) && pdist([x y;0.5 0.5]) < pdist([x y;1 1])
      count++;
      plot(x, y, 'db');
    endif
  endfor

  out=count/n;
end
