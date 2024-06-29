function Q = adquad(f, a, b, err)
  % f - funcite de integrat
  % a,b - interval
  % err - eroare de calculare a integralei

  c = (a + b)/2;
  fa = f(a); fc = f(c); fb = f(b);
  Q = quadstep(f, a, b, err, fa, fc, fb);
end