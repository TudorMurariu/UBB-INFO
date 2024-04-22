function koch(pl,pr,level)
%KOCH   Curba Koch generata recursiv.
%       Apel KOCH(PL, PR, LEVEL) unde punctele PL si PR 
%       sunt extremitatea stanga si dreapta 
%       LEVEL este nivelul de recursivitate.

if level == 0
  plot([pl(1),pr(1)],[pl(2),pr(2)]); % Uneste pl si pr.
  hold on
else
  A = (sqrt(3)/6)*[0 1; -1 0];       % matrice rot./scal.

  pmidl = (2*pl + pr)/3;
  koch(pl,pmidl,level-1)             % ramura stanga

  ptop = (pl + pr)/2 + A*(pl-pr);
  koch(pmidl,ptop,level-1)           % ramura stanga-mijloc

  pmidr = (pl + 2*pr)/3;
  koch(ptop,pmidr,level-1)           % ramura dreapta-mijloc

  koch(pmidr,pr,level-1)             % ramura dreapta

end
