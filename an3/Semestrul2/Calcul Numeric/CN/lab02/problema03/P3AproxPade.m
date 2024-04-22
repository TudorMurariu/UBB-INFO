function fAprox = P3AproxPade(x, c, m, k)
% calculeaza aprximarea Pade rationala pentru o functie f
% - aproximarea se calculeaza in punctele x (vector)
% - functia de aproximat este reprezentata prin coeficientii c ai seriei MacLaurin trunchiate: 
%   c(i) = 1/i! * diff(f, x$(i-1))|x=0
% - m: gradul polinomului de la numaratorul aproximantei
% - k: gradul polinomului de la numitorul aproximaentei
% Observatie: 
% coeficientii c trebuie calculati pana la gradul m+k+1 => length(c) >= m+k+2
% Exemplu, pentru f(x) = exp(x) => c(i) = 1/i!, pentru i=1..m+k+1

if length(c) < m+k+2
    error('Sunt necesari cel putin %i coeficienti c', m+k+2)
end

C = zeros(k); % creaza o matrice k*k avand toate valorile initializate cu zero
y = zeros(k, 1); % creaza un vector coloana cu k elemente
for i = 1:k
	for j = 1:k
		if m+i-j>0
			C(i, j) = c(m+i-j+1);
		end
	end
	y(i) = -c(m+i+1);
end
b = C \ y; % rezolva sistemul C*b=y => gasim coeficientii b

b = [1; b]; % adaugam 1 pe prima coloana (b(1)=1, prin constructie)

a = zeros(m+1, 1);
for j=1:m+1
	for l=1:min(j, k+1) % pentru l>k+1 avem b(l) = 0
		a(j) = a(j) + c(j-l+1)*b(l);
	end
end

%a
%b

fAproxNumarator = polyval(a(m+1:-1:1), x);
fAproxNumitor = polyval(b(k+1:-1:1), x);
fAprox = fAproxNumarator./fAproxNumitor;

