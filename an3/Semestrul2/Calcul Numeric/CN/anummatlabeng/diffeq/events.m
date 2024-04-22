function [value,isterminal,direction] = events(t,y,k)
%EVENTS    Functie eveniment pentru FOX2.
%          Localizare cand vulpea este aproape de iepure.

r = sqrt(1+t)*[cos(t); sin(t)];
value = norm(r-y) - 1e-4;     % vulpea aproape de iepure.
isterminal = 1;               % oprire integrare.
direction = -1;               % valoarea descreste catre 0.
