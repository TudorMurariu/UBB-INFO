%cycletest
f1=@(x) tan(x)-2*x;
f1d=@(x) cos(x).^(-2)-2;
[x0,ni]=Newtons(f1,f1d,3*pi/7,0,eps,200)
[x,n2]=Newtons(@sin,@cos,x0,0,eps,200)
[x1,n3]=Newtons(@sin,@cos,x0-eps,eps,eps,200)