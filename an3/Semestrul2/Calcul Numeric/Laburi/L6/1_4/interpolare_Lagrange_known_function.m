function val = interpolare_Lagrange_known_function(x, f, m, nodes)

  addpath('../1_1');
  considered = nodes(1 : m);
  conseideredVals = f(considered);
  
  val = interpolare_Lagrange(considered, conseideredVals, x);

endfunction