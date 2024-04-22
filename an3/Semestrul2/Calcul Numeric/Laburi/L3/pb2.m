function pb2()
    % a)
    for n = 10:15
        k = 1:n;
        t = -1 + 2 * k/n;
        v = vander(t);
        nc = cond(v, 1);
        nc
    end

    % b)
    for n = 10:15
        k = 1:n;
        o = ones(1, n);
        % ./ - Element-wise right division
        t = o./k;
        v = vander(t);
        nc = cond(v, 1);
        nc
    end
end
