/*

cmmdc(A, B) = { A , B = 0 }
              { cmmdc(B, A % B) , altfel}


    cmmdc(A, B, R)
    A :  primul nr
    B : al 2-lea nr
    R : raspuns-ul (cmmdc-ul)

    Modele de flux : (i,i,o) (i,i,i)

while(a != 0)
{
    r = a % b;
    b = a;
    a = r;
}

*/

cmmdc(A, 0, A):- !.

cmmdc(A, B, R):- A1 is A mod B,
                 cmmdc(B, A1, R).


/*

cmmdc_list(l1..ln, C) = { 1, n = 0 }
                        { l1, n = 1 }
                        { cmmdc_list(l2..ln, cmmdc(l1, C)), altfel }

    cmmdc_list(L, C, R)
    L : lista data
    C : o variablia auxiliara
    R : raspunsul

    Modele de flux : (i,i,o) (i,i,i)

*/

cmmdc([], 1):- !.
cmmdc_list([H|T], R):- cmmdc_list(T, H, R).

cmmdc_list([], C, C):- !.
cmmdc_list([H|T], C, R):- cmmdc(H, C, C1),
                          cmmdc_list(T, C1, R).