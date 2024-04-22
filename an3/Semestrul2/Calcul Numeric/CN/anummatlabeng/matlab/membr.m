Z=membrane; FS = 'FontSize';
subplot(2,2,1), surf(Z), title('\bf{surf}',FS,14), colorbar
subplot(2,2,2), surfc(Z), title('\bf{surfc}',FS,14), colorbar
subplot(2,2,3), surf(Z), shading flat
                title('\bf{surf} shading flat',FS,14), colorbar
subplot(2,2,4), waterfall(Z), title('\bf{waterfall}',FS,14), colorbar