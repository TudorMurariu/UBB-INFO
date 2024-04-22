h=10.^[0:-1:-4];
taylerr=abs((1+h+h.^2/2)-exp(h));
loglog(h,taylerr,'-',h,h.^3,'--')
xlabel('h'), ylabel('|eroare|')
title('Eroarea in aproximarea Taylor de gradul 2 a lui exp(h)')
box off
