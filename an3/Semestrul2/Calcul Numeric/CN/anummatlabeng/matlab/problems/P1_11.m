function B=P1_11(A,nk)
%P1_11 - extract blocks from diagonal of A
%A - matrix
%nk - block size (two-line matrix)

m=size(A);
if ~isequal(sum(nk,2),m')
    error('ilegal block size')
end
B=A(1:nk(1,1),1:nk(2,1))
pk=1+nk(:,1);
for k=2:length(nk);
    B=blkdiag(B,A(pk(1):pk(1)+nk(1,k)-1,pk(2):pk(2)+nk(2,k)-1));
    pk=pk+nk(:,k);
end
