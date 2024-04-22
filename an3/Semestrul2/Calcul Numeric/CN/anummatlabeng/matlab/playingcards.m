%PLAYINGCARDS
%Simulating a card game

rand('twister',sum(100*clock));
for k=1:20
   n=ceil(13*rand);
   fprintf('Selected card: %d\n',n)
   disp('  ')
   disp('Press  r and Return to continue')
   r=input('or any other key to finish: ','s');
   if r~='r', break, end
end