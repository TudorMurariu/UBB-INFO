%exemple9_5_10
load exampleev
tol=10.^[-10,-20,-30];
for k=1:length(tol)
    [l,it1]=QRSplit2(A1,tol(k));
    [l,it2]=QRSplit3(A1,tol(k));
    [l,it3]=QRSplit2(A2,tol(k));
    [l,it4]=QRSplit3(A2,tol(k));
    fprintf('%g & %d & %d & %d & %d \\\\ \n', tol(k),it1,it2,it3,it4)
end