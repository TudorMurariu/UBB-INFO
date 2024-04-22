function [sin] = pade_sin(x, k, m)
	x = rem(x, 2*pi);
	n = k+m+1;
	cSin = [0, 1]; % coeficientii MacLaurin pt. sin
	coefK = 1; % k!
	for i = 1: n
		coefK = - coefK * (2*i) * (2*i+1);
		cSin = [cSin, 0, 1/coefK];
	end
	sin = pade_aprox(x, cSin, m, k);
end
