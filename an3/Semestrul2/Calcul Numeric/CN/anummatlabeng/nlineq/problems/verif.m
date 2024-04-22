for k=0:9
    subplot(2,5,k+1)
    x=linspace(pi/2+k*pi+pi/100,pi/2+(k+1)*pi-pi/100,100);
    plot(x,tan(x),x,x); 
end