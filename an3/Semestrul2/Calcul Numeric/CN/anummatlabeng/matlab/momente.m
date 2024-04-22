function varargout = momente(x)
%MOMENTE Momentele unui vector.
%        [m1,m2,...,m_k] = MOMENTE(X) returns momentele de
%        ordin 1, 2, ..., k ale vectorului  X, unde momentul
%        de ordin j este SUM(X.^j)/LENGTH(X).

for j=1:nargout, varargout{j} = {sum(x.^j)/length(x)}; end

