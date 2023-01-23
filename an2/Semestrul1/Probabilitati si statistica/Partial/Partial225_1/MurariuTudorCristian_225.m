function rezolvare
  #R1.2
   U1="aaaaaarrrrrrrrrrbbbbbbbbbvvvvv";
   U2="aaaaaaarrrrbbbbbbvvvvvvvv";

   A=0;
   B=0;
   C=0;

   N=100;
   for i=1,N,
     albe=0;
     una_verde=0;
     rosii=0;
     A1=U1;
     A2=U2;

       for j=1,6,
         x = randi(length(A2));
         A1 = [A1, A2(x)];
       endfor

       for j=1,3,
         x = randi(length(A1));

        if A1(x) == 'v',
          una_verde = 1
        endif

        if A1(x) == 'a',
          albe++;
        endif

        if A1(x) == 'r',
          rosii++;
        endif

        #A1=erase(A1(x));

       endfor

    if una_verde == 1,
      B++;
    endif

    if albe == 0,
      A++;
    endif

    if rosii == 3,
      C++;
    endif

   endfor

   #disp(A);
   #disp(B);
   #disp(C);

  disp("\n");

   disp(A/N);
   disp(B/N);
   disp(C/N);
end

