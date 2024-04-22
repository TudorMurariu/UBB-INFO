function [med,abmp] = stat(x)
%STAT  Mean and standard deviation of a sample
%      [MEAN,SD] = STAT(X) computes mean and standard
%      deviation of sample X

n = length(x);
mean = sum(x)/n;
sd = sqrt(sum((x-med).^2)/n);