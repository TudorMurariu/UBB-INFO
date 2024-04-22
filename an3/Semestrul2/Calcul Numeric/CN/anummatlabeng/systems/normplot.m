[X,Y]=meshgrid(linspace(-1.2,1.2,100));
p=[1,2,3,Inf];
for k=1:4
    subplot(1,4,k);
    Z=mynorm(X,Y,p(k));
    contour(X,Y,Z,[1,1],'k-');
    axis square
    if isfinite(p(k))
        st=num2str(p(k));
    else
        st='\infty';
    end
    title(['p = ' st],'FontSize',12)
    box off
end