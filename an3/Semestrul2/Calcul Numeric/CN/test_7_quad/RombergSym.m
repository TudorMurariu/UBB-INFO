function R = RombergSym(f, a, b, n = 3)
  % ROMBERGSYM - calcul simbolic pentru metoda lui Romberg
  % call R = RombergSym(f, a, b, n)
  % f - functia
  % a, b - capetele intevalului
  % n - numarul de iteratii

  R = sym(zeros(n, n));
  h = b - a;

  % prima iteratie
  R(1, 1) = h / 2 * (sum(f([a, b])));

  for k = 2: n
     % formula trapezului
     x = a + ([1: 2 ^ (k - 2)] - sym(1) / sym(2)) * h;
     R(k, 1) = sym(1) / sym(2) * (R(k - 1, 1) + h * sum(f(x)));

     % extrapolare
     plj = sym(4);
     for j = 2: k
        R(k, j) = (plj * R(k, j - 1) - R(k - 1, j - 1)) / (plj - 1);
        plj = plj * sym(4);
     endfor

     %halving step
     h = h / sym(2);
  endfor
endfunction
