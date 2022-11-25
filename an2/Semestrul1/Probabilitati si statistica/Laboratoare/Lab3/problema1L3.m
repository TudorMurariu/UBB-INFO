function out=problema1L3()
  urn='rrrrraaavv';
  sim = 5000;
  pA=0; pAB=0; Pcomd=0;

  for i=1:sim
      ext=randsample(urn, 3);
      if any(ext=='r')
        pA++;
      endif
      if all(ext=='r')
        pAB++;
      endif
  endfor

  %disp(pA/sim); % i
  %disp(pAB/sim); % ii
  out= pAB/pA;  % iii
end

