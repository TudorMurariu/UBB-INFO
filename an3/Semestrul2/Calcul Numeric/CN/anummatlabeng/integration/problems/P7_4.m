%P7_4 - calculul functiei erf
erfun=@(t) exp(-t.^2);
tab=[];
for x=0.1:0.1:1
    tab=[tab;x,2/sqrt(pi)*[adquad(erfun,0,x),quad(erfun,0,x)],erf(x)];
end
tab    