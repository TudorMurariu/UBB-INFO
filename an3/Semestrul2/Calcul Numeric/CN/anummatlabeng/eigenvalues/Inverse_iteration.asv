function [x,lambda,ni]=Inverse_iteration(A,x0,sigma,err)
%INVERSE_ITERATION - computes an eigenvector
%call [x,lambda,ni]=Inverse_iteration(A,x0,sigma,err)
%Input
%A - matrix
%x0 - starting vector
%sigma - aproximate ew
%err - tolerance
%Output
%x - eigenvector
%lambda - eigenvalue

sI=sigma*eye(size(A)); ni=0;
warning off
while 1
    y=(A-sI)\x0;
    x=y/norm(y);
    lambda=x'*A*x;
    ni=ni+1;
    if (norm(x-x0)<err*norm(x)) || (norm(x+x0)<err*norm(x)) 
        warning on
        return
    end
    x0=x;
end


