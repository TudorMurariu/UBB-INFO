[X,Y]=meshgrid(linspace(-1.2,1.2,500));
p=[1,2,3,Inf];
for k=1:4
    subplot(1,4,k);
    Z=mynorm(X,Y,p(k));
    contour(X,Y,Z,[1,1],'k-');
    axis square
    title(['p=' num2str(p(k))],'FontSize',12)
    box off
end