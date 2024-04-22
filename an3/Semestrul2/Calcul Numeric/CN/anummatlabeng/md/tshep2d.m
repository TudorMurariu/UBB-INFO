function z=tshep2d(X,Y,P,f,mu)
%TSHEP2D test Shepard 2D
%call z=tshep2d(X,Y,P,f,mu)
[mx,nx]=size(X);
for i=1:mx
    for j=1:nx
        z(i,j)=Shepards([X(i,j);Y(i,j)],P,f,mu);
    end
end

    
    
    
    
        
        