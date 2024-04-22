azrange=-60:20:0;
elrange=0:30:90;
spr=length(azrange);
spc=length(elrange);
pane=0;
for az=azrange
    for el=elrange
        pane=1+pane;
        subplot(spr,spc,pane);
        [x,y,z]=peaks(20);
        mesh(x,y,z);
        view(az,el);
        tstring=['Az=',num2str(az),...
            ' El=',num2str(el)];
        title(tstring)
        axis off
    end
end