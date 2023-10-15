# Lab 1

## Enunt general:
Scrierea unui ANALIZOR LEXICAL pentru un minilimbaj de programare (MLP),
ales ca subset al unui limbaj existent

### Specificarea Minilimbajului de Programare (MLP):

**Tipuri de Date:**
1. Integer: Tip de date reprezentând numere întregi.
2. Float: Tip de date reprezentând numere reale
3. String: Tip de date reprezentând un sir de litere


**Instrucțiuni:**
1. Atribuire (Assignment): variabilă ⬅️ expresie;
2. Intrare/Ieșire (Input/Output): 🖨️(expresie); / citeste(variabilă);
3. Selecție (Conditional):
```
❓ (condiție) {
    // bloc de instrucțiuni
} altfel {
    // bloc de instrucțiuni
}
```
4. Ciclare (Loop):
```
🔁 (condiție) {
    // bloc de instrucțiuni
}
```
**Restricții:**
Identificatori: Pot conține litere, cifre și underscore (_) și trebuie să înceapă cu o literă. Nu pot fi identici cu cuvintele cheie.


### Analizor Lexical:

1. Simboluri:
    - operators:
		* arithmetic: +, -, *, /, %
		* assignment: ⬅️
        * equality testing: ==, !=
        * order relations: <, <=, >, >=
		* sequencing: ","
    - separators { }  ; ( )
    - Cuvinte cheie: 
       * int      = int
       * float    = float
       * string   = string
       * 🖨️      = cout <<
       * citeste  = cin >>
       * ❓       = if  (nu sunt sigur daca e simbol sau cuvant cheie)
       * altfel   = else
       * 🔁      = while
  
2. Identificatori
    - identifier = letter [{letter | digit | "_"}]*
    - letter = "A" | "B" | . ..| "Z" | "a" | "b" | ... | "z"
    - non_zero_digit = "1" |...| "9"
    - zero_digit = "0" 
    - sign = ["+" | "-"]
    - comma = ","

**Codification table:**
```
ATOM    : COD
ID      : 0
CONST   : 1
(       : 2
)       : 3
,       : 4
int     : 5
float   : 6
string  : 7
{       : 8
}       : 9
;       : 10
⬅️      : 11
🖨️      : 12
citeste : 13
+       : 14
-       : 15
*       : 16
/       : 17
%       : 18
🔁      : 19
!=      : 20
==      : 21
<       : 22
>       : 23
<=      : 22
>=      : 25
❓      : 26
🏁      : 27
```

**Mapa Semne Regex:**
```
. = orice
| = sau
? = one ore zero times
+ = one or any amount of times
* = zero or any amount of times
() = grup
{} = specifici de cate ori
```


```html
<program> ::= <antet_program> <functie>
<antet_program> ::= 🏁 | 🏁 <declarare>
<functie> ::= <antet_functie> <corp>
<antet_functie> ::= <tip> ID (<lista_decl>) | <tip> ID ()
<tip> ::= int | float | string
<lista_decl_fct> ::= <declarare_fct> , <lista_decl_fct> | <declarare_fct>
<declarare_fct> ::= <tip> ID
<corp> ::= { <instr_compusa> }
<instr_compusa> ::= <instr> <instr_compusa> | <instr>
<instr> ::= <declarare> | <atribuire> | <instr_citire> | <instr_afisare> | <instr_while> | <instr_if>
<declarare> ::= <tip> ID ; | <tip> <atribuire>
<atribuire> ::= ID = <expr_aritmetica> ;
<instr_citire> ::= citeste ( ID ) ;
<instr_afisare> ::= 🖨️ ( <expr_aritmetica> ) ;
<expr_aritmetica> ::= <expr_aritmetica> <op_artimetic> <expr_aritmetica> | ID | CONST
<op_artimetic> ::= + | - | * | / | %
<instr_while> ::= 🔁 ( <conditie> ) <corp>
<conditie> ::= <expr_aritmetica> <op_relational> <expr_aritmetica> | <expr_aritmetica>
<op_relational> ::= != | == | < | > | <= | >=
<instr_if> ::= ❓ ( <conditie> ) <corp>

ID ::= (_ | a | b | ... | z | A | B | ... | Z)(_ | a | b | ... | z | A | B | ... | Z | 0 | 1 | ... | 9){0,249}
CONST ::= <const_int> | <const_float> | <const_string>
<const_int> ::= (+ | -)?(0 | 1 | ... | 9)+
<const_float> ::= (+ | -)?(0 | 1 | ... | 9)+ | (+ | -)?(0 | 1 | ... | 9)+\.(0 | 1 | ... | 9)*
<const_string> ::= ".*"

ID ::= ^[_a-zA-Z]([_a-zA-Z0-9]){0,249}$
CONST ::= <const_int> | <const_float> | <const_string>
<const_int> ::= ^[+-]?[0-9]+$
<const_float> ::= ^[+-]?[0-9]+(\.[0-9]*)?$
<const_string> ::= ^".*"$
```

### 2. se cer textele sursa a 3 mini-programe  

1. calculeaza perimetrul si aria cercului de o raza data data
```cpp
🏁
int main() {
    float raza;
    citire (raza);

    float pi = 3.14;

    float P = 2 * pi * raza;
    float A = pi * raza * raza;

    🖨️ (P);
    🖨️ (" ");
    🖨️ (A);
}
```

2. determina cmmdc a 2 nr naturale
```cpp
🏁

int main() {
    int a;
    int b;
    
    citire (a);
    citire (b);

    int r = -1;
    🔁 (r != 0) {
        r = a % b;
        a = b;
        b = r;
    }

    🖨️ (a);
}
```

3. calculeaza suma a n numere citite de la tastatura 
```cpp
🏁

int main() {
    int sum = 0;
    int n;
    int x;
    
    citire (n);
    while (n > 0)
    {
        citire (x);
        sum = sum + x;
        n = n - 1;
    }

    🖨️ (sum);
}
```

### 3. Se cer textele sursa a doua programe care contin erori conform MLP-ului definit:

1. Unul dintre programe contine doua erori care sunt in acelasi timp erori in limbajul original (pentru care MLP defineste un subset)

```cpp
🏁

int main() {
    float a // eroare

    a = 15.4;

    in b; // eroare
    citire (b);

    cout << a + 2;
}
```

2. Al doilea program contine doua erori conform MLP, dar care nu sunt erori in limbajul original. Se cere ca acesta sa fie compilat si executat in limbajul original ales

```cpp
🏁

int main() {
    int a, b; // eroare - fiecare pe o alta linie in MLP

    cin >> a;
    cin >> b;

    cout << a + b;

    return 0; // eroare - return nedefinit
}
```