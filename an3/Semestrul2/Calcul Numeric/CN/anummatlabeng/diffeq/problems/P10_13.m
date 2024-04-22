%P10_13
md=@(x,y) y;
opts=odeset('Stat','on');
[tout,yout] = oderk(md,[0,1],1,@EulerHeun,opts);