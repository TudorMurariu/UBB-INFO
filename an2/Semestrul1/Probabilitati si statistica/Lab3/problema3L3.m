function out=problema3L3()

  n = 1000;

  t = randi(6, 4, n);
  sume_pos=4:24;
  sume_sim=sum(t);
  frecv_abs = hist(sume_sim, sume_pos);

  out=[sume_pos; frecv_abs];
end
