# Lab 1 PPD

## Fie: 
* NxM dimensiunea matricii
* p numarul de thread uri
* nxm dimensiunea matricii de convolutie

## Java si C++ (static + dinamic)
1. linii
```
    Impartim matricea in linii, fiecare thread va avea N/p linii
    Vom adauga cate o linie din restul N%p de linii la cate un thred pana restul devine 0.
```
2. coloane
```
    Impartim matricea in coloane, fiecare thread va avea N/p coloane
    Vom adauga cate o coloana din restul N%p de coloane la cate un thred pana restul devine 0.
```
3. vectorizare
```
    Tratam matricea ca pe un vector.
    elementsPerThread = totalElements / p;

    Iteram thread urile:  for(int k=0;k<p;++k)
    startIndex = k * elementsPerThread + cate elemente din rest am tratat
    endIndex = (k + 1) * elementsPerThread (+1 daca mai avem elemente in rest netratate)

    In thread iteral de la startIndex si endIndex si facem rost de i si j:
    i = index / N;
    j = index % N;
```
   

## Observatii 
1. C++ vs Java
```
C++ este mai rapid pe cazurile cu matrici mici dar pe matricile mai mari exista cazuri in care programul in Java a fost mai rapid
```
2. Nr de Threaduri
```
De cele mai multe ori (exceptand matricile mici) mai multe threduri => program mai rapid
```
3. Coloane == Linii
```
Rezolvarea pe coloeane si ce pe linii au aproape exact aceeasi timpi de executie
```
4. Vectorizare > Linii
```
De cele mai multe ori algoritmul de vectorizare este mai rapid decat cel pe linii sau coloane
```
5. Static >= Dinamic
```
In cpp programul cu implementare statica a fost mai rapid
Alocarea memoriei este probabil problema.
```