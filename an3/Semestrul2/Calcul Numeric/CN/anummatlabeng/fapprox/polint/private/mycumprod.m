function p=mycumprod(v)
for k=1:length(v)
    p(k)=prod(v(1:k));
end