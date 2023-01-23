function out=p226_1()
  # a)
  v=zeros(1000,1);
  nr=0;
  for i=1:1000,
    x=normrnd(100,20);
    v(i)=x;
    # disp(x);
    if x > 80 && x < 140,
      nr++;
    endif
  endfor

  disp("Probabilitatea este aprox: ");
  disp(nr/1000);

  # b)
  disp("Probabilitarea teoretica :")
  disp(normcdf(140, 100, 20) - normcdf(80, 100, 20));

  # c)
  [n, xout] = hist(v, 13);
  bar(xout, n/sum(n));
  hold on;
  x = 80:0.1:140;
  y = normpdf(x, 100, 20);
  plot(x, y, 'r');
  hold off;
end
