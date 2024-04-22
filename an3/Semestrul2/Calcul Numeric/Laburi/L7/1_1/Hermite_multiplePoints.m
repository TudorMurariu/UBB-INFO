
function results = Hermite_multiplePoints(x, f, fderiv, points)

  results = zeros(size(points));
  for i = 1 : length(points)
    results(i) = Hermite(x, f, fderiv, points(i));
  endfor

endfunction