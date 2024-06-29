timp = [0 3 5 8 13];
dist = [0 225 383 623 993];
viteza = [75 77 80 74 72];

distMoment10 = hermite(timp, dist, viteza, 10)
vitMoment10 = distMoment10 / 10