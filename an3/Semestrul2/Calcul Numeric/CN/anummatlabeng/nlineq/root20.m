%root 20th
f=@(x) x^20-1;
fd=@(x) 20*x^19;
[x,n]=Newtons(f,fd,1/2,0,eps,500)