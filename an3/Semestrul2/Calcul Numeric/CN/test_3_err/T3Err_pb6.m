function T3Err_pb6()
  # care este “limita in aritmetica masinii”? Explicati. 
  # "limita in aritmetica masinii" = limita sirului daca masina ar incerca sa calculeze sirul manual
  
  val_curenta=0;
  val_precedenta=-1;
  # n trebuie sa ia valoarea maxima pentru o aproximare cat mai buna
  n=realmax();
  while val_curenta!=val_precedenta
    val_precedenta=val_curenta;
    val_curenta=(1+1/n)^n;
    n=n+1;
  endwhile

  disp("Valoare calculata:");
  double(val_curenta)

  disp("Valoare lui e:");
  double(exp(1))
  eps()

  # Explicatie: 
  # 1/n cand n devine suficient de mare poate deveni atat de apropiat de zero incat 
  # din cauza limitarilor aritmeticii cu precizie finita numarul 1/n este aproximativ zero
  # in loc sa obtinem valoarea corecta a expresiei 1/n, obtinem o valoare mai mica decat cea a preciziei masinii(eps) 
  # de aici vom avea 1 + 1/n == 1 => limita va fi egala cu 1
endfunction

