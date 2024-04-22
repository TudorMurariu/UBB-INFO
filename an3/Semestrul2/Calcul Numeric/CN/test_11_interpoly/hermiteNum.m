function ff = hermiteNum(xx, x, y, yd)
    if (nargin > 3)
        [z, T] = divdiffdn(x, y, yd);
        ff = Newtonpol(T, z, xx);
    else
        T = divdiff(x, y);
        ff = Newtonpol(T, x, xx);
    end
end