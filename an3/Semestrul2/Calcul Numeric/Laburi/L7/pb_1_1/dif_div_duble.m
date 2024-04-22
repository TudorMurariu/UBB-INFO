
function T = dif_div_duble(x, f, df)

	T = NaN(2 * length(x));
	z = repelem(x, 2);
	T(:, 1) = repelem(f, 2);
	T(1 : 2 : end - 1, 2) = df;
	T(2 : 2 : end - 2, 2) = diff(f) ./ diff(x);
	for j = 3 : length(z)
		 T(1 : end - j + 1, j) = diff(T(1 : end - j + 2, j - 1)) ./ (z(j : end) - z(1 : end - j + 1))';
    end

end
