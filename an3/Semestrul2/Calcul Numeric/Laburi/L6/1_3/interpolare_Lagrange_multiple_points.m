

function [vals] = interpolare_Lagrange_multiple_points(nodes, nodevals, points)
  addpath('../1_1');
  vals = zeros(size(points));
  [~, col] = size(points); 
  for i = 1 : col
    vals(1, i) = interpolare_Lagrange(nodes, nodevals, points(1, i));
  endfor
  
endfunction