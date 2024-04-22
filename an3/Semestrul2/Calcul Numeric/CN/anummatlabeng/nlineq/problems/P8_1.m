%P8_1 gasiti primele 10 valori pozitive pentru care tg(x)=x
f1=@(x) tan(x)-x;
f1d=@(x) tan(x).^2;
x0=[3*pi/2:pi:21*pi/2]-pi/150;
for k=1:length(x0)
    z(k)=Newtons(f1,f1d,x0(k),1e-6,0,100);
end
z
    