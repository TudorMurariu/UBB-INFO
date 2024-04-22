function [pSin, pCos] = Problema3(x, k, m)
% - m: gradul polinomului de la numaratorul aproximantei
% - k: gradul polinomului de la numitorul aproximaentei
	x = rem(x, 2*pi);
	n = k+m+1;
	cSin = [0, 1]; % coeficientii MacLaurin pt. sin
	cCos = [1]; % coeficientii MacLaurin pt. cos
	coefK = 1; % k!
	for i = 1: n
		coefK = - coefK * (2*i); 
		cCos = [cCos, 0, 1/coefK];
		coefK = coefK * (2*i+1); 
		cSin = [cSin, 0, 1/coefK];
	end
	pSin = Problema3(x, cSin, m, k)
	pCos = Problema3(x, cCos, m, k)
	