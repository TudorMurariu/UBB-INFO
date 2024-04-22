%P7_6
syms t fs vai va
ITT=[]; IJT=[]; va=[];
for p=5:5:20
    f1=@(x) (1-x).^p.*tan(x);
    fs=(1-t).^p.*tan(t);
    vai=int(fs,t,0,1);
    va=[va,double(vai)];
    %trapeze
    IT=[];
    for n=10:10:50
        vi=trapez(f1,0,1,n);
        IT=[IT,vi];
    end
    ITT=[ITT;IT];
    %Jacobi
    IJ=[];
    for n=1:5
        [x,A]=Gauss_Jacobi(n,p,0);
        vi=1/2^(p+1)*(A*tan((x+1)/2));
        IJ=[IJ,vi];
    end
    IJT=[IJT;IJ];
end
real(va)
ITT
IJT
disp('eroare trapeze')
disp(real(va)'-ITT(:,5))
disp('eroare Jacobi')
disp(real(va)'-IJT(:,5))
