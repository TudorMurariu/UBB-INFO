[X,Y]=meshgrid(linspace(-1,1,20)); Z=X.^2-Y.^2;
FS = 'FontSize';
subplot(2,2,1), surf(X,Y,Z), 
                title('\bf{surf}',FS,14), colorbar
subplot(2,2,2), surfc(X,Y,Z), 
                title('\bf{surfc}',FS,14), colorbar
subplot(2,2,3), surf(X,Y,Z), shading flat
                title('\bf{surf} shading flat',FS,14), colorbar
subplot(2,2,4), waterfall(X,Y,Z)
                title('\bf{waterfall}',FS,14), colorbar