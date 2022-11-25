function out=p2b()
  k = 10;
  p = 0.2;
  v = [];
  m = 100;
  for i=1:m
    v = [v, p2a(k, p)(end)];
  endfor
  h = hist(v, -k:k);
  bar(-k:k, h, 'hist', 'FaceColor', 'k');

  deplasariPosibile = -k:k;
  hMax = max(h);

  #fprintf("Pozitia finala este : %d", deplasariPosibile(h == hMax));
  out = find(h == hMax) - k - 1;

  #out = v;
  # nu inteleg, dar merge
end

