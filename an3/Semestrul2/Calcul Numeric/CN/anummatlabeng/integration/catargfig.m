
% Create figure
figure1 = figure('PaperSize',[20.98 29.68]);

% Create axes
axes1 = axes('Visible','off','Parent',figure1,...
    'PlotBoxAspectRatio',[378.2 351.3 87.82],...
    'DataAspectRatio',[1 1 1]);
box('on');
hold('all');

% Create plot
plot(X1,Y1,'LineWidth',4,'Color',[0 0 0]);

% Create plot
plot(X2,X1,'LineStyle','--','Color',[0 0 0]);

% Create text
text('Parent',axes1,'String','L','Position',[5.226 5.149 67.07],...
    'FontSize',14);

% Create text
text('Parent',axes1,'String','A','Position',[-0.5712 0.6236 17.32],...
    'FontSize',14);

% Create text
text('Parent',axes1,'String','B','Position',[-0.5 8 0],'FontSize',14);

% Create text
text('Parent',axes1,'String','T','Position',[-2.5 3 0],'FontSize',14);

% Create text
text('Parent',axes1,'String','O','Position',[-3.8 0.009943 67.07],...
    'FontSize',14);

% Create plot
plot(X3,Y2,'Color',[0 0 0]);

% Create textbox
annotation(figure1,'textbox','String',{'H'},'FontSize',14,...
    'FontName','Times New Roman',...
    'FontAngle','italic',...
    'LineStyle','none',...
    'Position',[0.6434 0.1243 0.05834 0.07657]);

% Create textbox
annotation(figure1,'textbox','String',{'\itb'},'FontSize',14,...
    'FontName','Times New Roman',...
    'LineStyle','none',...
    'Position',[0.7131 0.289 0.05292 0.07657]);

% Create textbox
annotation(figure1,'textbox','String',{'M'},'FontSize',14,...
    'FontName','Times New Roman',...
    'FontAngle','italic',...
    'LineStyle','none',...
    'Position',[0.6107 0.2983 0.06106 0.07657]);

% Create textbox
annotation(figure1,'textbox','String',{'R'},'FontSize',14,...
    'FontName','Times New Roman',...
    'FontAngle','italic',...
    'LineStyle','none',...
    'Position',[0.6311 0.4677 0.05563 0.07657]);

% Create textbox
annotation(figure1,'textbox','String',{'\itV'},'FontSize',14,...
    'FontName','Times New Roman',...
    'LineStyle','none',...
    'Position',[0.4713 0.1011 0.05563 0.07657]);

% Create arrow
annotation(figure1,'arrow',[0.5455 0.5451],[0.09745 0.1949]);

% Create arrow
annotation(figure1,'arrow',[0.5455 0.3935],[0.9165 0.2088]);

% Create arrow
annotation(figure1,'arrow',[0.6142 0.5814],[0.2867 0.2923]);

% Create arrow
annotation(figure1,'arrow',[0.7658 0.5506],[0.2009 0.2019]);

% Create doublearrow
annotation(figure1,'doublearrow',[0.7576 0.7576],[0.2088 0.4501]);

% Create doublearrow
annotation(figure1,'doublearrow',[0.8548 0.8548],[0.9188 0.2019]);

% Create arrow
annotation(figure1,'arrow',[0.5459 0.8548],[0.4491 0.4501]);

