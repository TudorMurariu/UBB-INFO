function y=betterSinPade(x, val)
  [val, semn]=reduceZeroPiPerTwo(val);
  y=subs(pade_aprox(sin(x), 2, 2, x), x, val);
end
