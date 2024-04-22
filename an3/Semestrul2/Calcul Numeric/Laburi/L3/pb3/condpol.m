function nc = condpol(p,r)
% conditionarea unei ecuatii polinomiale p(x)=0

    if nargin < 2 % daca nu sunt primite radacinile ca parametru
        % se calculeaza radacinile polinomului
        r = roots(p);
    end
    
    % calculam derivata p'(x)
    n = length(p)-1;                  
    dp = [n:-1:1] .* p(1:end-1);
    val_df = polyval(dp,r); % valoarea derivatei in fiecare radacina
    poliv = polyval(abs(p(2:end)),abs(r));
    nc = poliv./(abs(r.*val_df));
end