function lambda=Eigen2x2(A)
%EIGEN2X2 - Compute eigenvalues of a 2x2 matrix
%A - 2x2 matrix
%lambda - eigenvalues

b=trace(A); c=det(A);
d=b^2/4-c;
if d > 0
    if b == 0
        lambda = [sqrt(c); -sqrt(c)];
    else
        x = (b/2+sign(b)*sqrt(d));
        lambda=[x; c/x];
    end
else
    lambda=[b/2+i*sqrt(-d); b/2-i*sqrt(-d)];
end