function x = quadeqn(a,b,c)

    function discr(a,b,c)
        d = sqrt(b().^2-4*a(:).*c(:));
    end %discr()
denom = 2*a(:);
discr(a,b,c);  % Root of the discriminant
x1 = (-b + d)./denom;
x2 = (-b - d)./denom;
x = [x1(:), x2(:)];
end %quadecn()
