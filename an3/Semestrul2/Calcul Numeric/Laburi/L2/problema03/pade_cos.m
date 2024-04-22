function [cos] = pade_cos(x, k, m)
	x = rem(x, 2*pi);
	n = k+m+1;
	cCos = [1]; % coeficientii MacLaurin pt. cos
	coefK = 1; % k!
	for i = 1: n
		coefK = - coefK * (2*i);
		cCos = [cCos, 0, 1/coefK];
		coefK = coefK * (2*i+1);
	end
	cos = pade_aprox(x, cCos, m, k);
end
