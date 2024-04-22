function  P1_3(n)
%P1_3
q=floor(n/1000);
r=mod(n,1000);
if q>=1
    rez=double('M')*ones(1,q);
end
if r>=900
    r=r-900;
    rez=[rez,double('C'),double('M')];
elseif r>=500
    r=r-500;
    rez=[rez,double('D')];
end
q=floor(r/100);
r=mod(r,100);
if q>=1
    rez=[rez,double('C')*ones(1,q)];
end
if r>=90
    r=r-90;
    rez=[rez,double('X'),double('C')];
elseif r>=50
    r=r-50;
    rez=[rez,double('L')];
end
q=floor(r/10);
r=mod(r,10);
if q>=1
    rez=[rez,double('X')*ones(1,q)];
end
if r>=9
    r=r-9;
    rez=[rez,double('I'),double('X')];
elseif r>=5
    r=r-5;
    rez=[rez,double('V')];
end
if r>=1
    rez=[rez,double('I')*ones(1,r)];
end
fprintf('%s\n',char(rez))
