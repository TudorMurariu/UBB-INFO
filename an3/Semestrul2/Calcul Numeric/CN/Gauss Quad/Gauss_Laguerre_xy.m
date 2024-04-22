function res = Gauss_Laguerre_xy(x, y, n)
    f = @(u) 1 ./ (u + x * y);
    [g_nodes, g_coeff] = Gauss_Laguerre(n, 0);
    f_values = f(g_nodes);
    res = sum(g_coeff' .* f_values);
end
