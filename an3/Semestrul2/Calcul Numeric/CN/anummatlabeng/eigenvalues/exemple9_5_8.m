%examplel9_5_8
load exampleev
te=[-5:-1:-10,-20];
tol=10.^te;
for k=1:length(tol)
    [l,it1]=QRMethod1(A1,tol(k));
    [l,it2]=QRMethod2(A1,tol(k));
    [l,it3]=QRSplit1(A1,tol(k));
    [l,it4]=QRSplit2(A1,tol(k));
    fprintf('%g & %d & %d & %d & %d \\\\ \n', tol(k), it1,it2,it3,it4)
end