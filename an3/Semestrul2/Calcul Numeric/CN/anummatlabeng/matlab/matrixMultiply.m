function matrixMultiply(A, B)
try
   X = A * B
catch
   err=lasterror;
   disp(err.identifier)
   disp(err.message)
end