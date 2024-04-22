count=1000;
results=[];
for i=1:count
  matrix_size = randi([100, 300]);
  A=randi(20, matrix_size, matrix_size); 
  tic;
  [L, U, P] = lup(A);
  fizical_premutations_time = toc;
  tic;
  [L, U, P] = lup2(A);
  logical_premutations_time = toc;
  results=[results; matrix_size, fizical_premutations_time, logical_premutations_time];
endfor
mean(results, 1)