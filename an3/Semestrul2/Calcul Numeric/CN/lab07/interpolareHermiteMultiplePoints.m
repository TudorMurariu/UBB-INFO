function [ vals ] = interpolareHermiteMultiplePoints( x, f, fd, points )
vals = zeros(size(points));
for i = 1 : length(points)
   vals(i) = interpolareHermite(x, f, fd, points(i)); 
end

endfunction
