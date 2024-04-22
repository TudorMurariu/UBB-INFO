function T=Chebyshevlc(deg)
%CHEBYSHEVLC - Chebyshev polynomials
%   tabulate coefficients of Chebyshev polynomials
%   T = CHEBYSHEVLC(DEG)
T = cell(1,deg);
T(1:2) = { [1], [1 0] };
for n = 2:deg 
    T{n+1} = [2*T{n} 0] - [0 0 T{n-1}]; 
end