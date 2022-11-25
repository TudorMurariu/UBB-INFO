function out=p1a
  # citire din fisier Yay

  #pt spam
  spamW = strsplit(fileread('keywords_spam.txt'), ' ');
  spamUW = unique(spamW);
  # primul element va fi un \n pe care nu il luam in calcul
  spamUW = spamUW(2:end); % de ce sunt mai multe keyworduri pt comentariu wtf
  emailOne = strsplit(fileread('email1.txt'), ' ');

  spamFq = [];
  spamL = length(spamW); # ~100 cuv
  spamUL = length(spamUW); # ~ 14 cuv

  for i=1:spamUL
    counter = 0;
    for j=1:spamL
      counter += strcmp(spamUW(i), spamW(j));
    endfor
    spamFq = [spamFq, counter / spamL];
  endfor

  #ham
  hamW = strsplit(fileread('keywords_ham.txt'), ' ');
  hamUW = unique(hamW);

  hamUW = hamUW(2:end); % de ce sunt mai multe keyworduri pt comentariu wtf
  emailTwo = strsplit(fileread('email2.txt'), ' ');

  hamFq = [];
  hamL = length(spamW); # ~100 cuv
  hamUL = length(spamUW); # ~ 14 cuv

  for i=1:hamUL
    counter = 0;
    for j=1:hamL
      counter += strcmp(hamUW(i), hamW(j));
    endfor
    hamFq = [hamFq, counter / hamL];
  endfor

  disp(hamFq);
  disp(spamFq);

  #########

  for i=1:spamUL
    counter = 0;
    for j=1:length(emailOne)
      counter += strcmp(spamUW(i), emailOne(j));
    endfor
    if counter == 0
      spamFq(i) = 1 - spamFq(i);
    endif
  endfor

  for i=1:hamUL
    counter = 0;
    for j=1:length(emailTwo)
      counter += strcmp(hamUW(i), emailTwo(j));
    endfor
    if counter == 0
      hamFq(i) = 1 - hamFq(i);
    endif
  endfor

  disp(prod(spamFq) * spamL / (spamL + hamL));
  disp(prod(hamFq) * hamL / (spamL + hamL));
end
