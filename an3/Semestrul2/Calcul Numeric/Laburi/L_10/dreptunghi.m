% Formula dreptunghiului
function I = dreptunghi(f,a,b,n)
    
    % call: I = dreptunghi(f,a,b,n)
    
    h = (b-a) / n;
    s1 = f([0:n-1] * h + a);
    s2 = f([1:n] * h + a);
    s = (s1 + s2) / 2;
    I = h * (sum(s));
end