function matrixMultiply2(A, B)
try
    A * B
catch
    errmsg = lasterr;
    err=lasterror;
    if(strfind(errmsg, 'Inner matrix dimensions'))
        disp('** Wrong dimensions for matrix multiply')
    elseif(strfind(errmsg, 'Undefined function or method'))
        disp('** Both arguments must be double matrices')
    end
end
end