%P10_14 ghiulea sferica
sol=x10_14;
wind={'none','head','tail','gusty'};
for tv=1:4
    figure(tv);
    plot(sol(tv,1).x,sol(tv,1).y);
    hold on
    bataie=sol(tv,1).ye(1);
    for k=2:17;
        plot(sol(tv,k).y(1,:),sol(tv,k).y(2,:));
        [bmax,i]=max(sol(tv,k).ye(1,:));
        bataie=[bataie,sol(tv,k).ye(1)];
    end
    
    title(wind{tv})
    hold off
    [bm,ii]=max(bataie);
    jj=find(bataie==bm);
    for ii=jj
        fprintf('k=%d\n',ii);
        fprintf('bataia maxima %f m\n',bm)
        fprintf('unghiul initial: %f grade\n',ii*180/36)
        fprintf('timpul de zbor: %f s\n',sol(tv,ii).xe)
        fprintf('viteza la impact: %f m/s\n',sol(tv,ii).ye(4))
        sol(tv,ii).stats
    end
    %pause
end
