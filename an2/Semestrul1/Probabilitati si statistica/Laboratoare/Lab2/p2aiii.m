function out=p2aiii(n)
  clf;
  hold on;
  axis square;
  # uhh

  rectangle('Position', [0 0 1 1]);
  count = 0;
  for i=1:n
    x = rand;
    y = rand;

    ascutie=0;
    optuze=0;

    if pdist([x y;0 0.5]) < 0.5
      ascutie++;
    else
      optuze++;
    endif

    if pdist([x y;0.5 0]) < 0.5
      ascutie++;
    else
      optuze++;
    endif

    if pdist([x y;1 0.5]) < 0.5
      ascutie++;
    else
      optuze++;
    endif

    if pdist([x y;0.5 1]) < 0.5
      ascutie++;
    else
      optuze++;
    endif

    if optuze == 2 && ascutie == 2
      plot(x, y, 'db');
      count++;
    endif

  endfor

  out=count/n;
end
