%example9_5_6
load exampleev
tol=10.^[-3:-1:-7];
for k=1:length(tol)
    [l,it]=QRSplit1a(A2,tol(k));
    fprintf('%g & %d & %7.4f ',tol(k),it,l(1))
    for jj=2:3
        fprintf('& %7.4f +%7.4fi ', real(l(jj)),imag(l(jj)))
    end
    fprintf('\\\\ \n')
end