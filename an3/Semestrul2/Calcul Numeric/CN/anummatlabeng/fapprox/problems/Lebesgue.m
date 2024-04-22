function y=Lebesgue(x,t)
%LEBESGUE - calculeaza functia lui Lebesgue pentru interpolare Lagrange
%apel y=Lebesgue(x,t)
%x - nodurile
%t - punctele
%y - valoarea

%calculez polinoamele fundamentale
Z=pfl2b(x,t);
y=sum(abs(Z));