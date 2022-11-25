function out=problema1L3iv()

  sim = 5000;

  sum = 0;
  for i=1:sim
    sum = sum + problema1L3();
  endfor

  out= sum/sim;
end
