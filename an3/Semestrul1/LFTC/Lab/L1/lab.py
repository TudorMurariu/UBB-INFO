
program = """
🏁

int main ( ) {
    int sum = 0 ;
    int n ;
    int x ;
    
    citire ( n ) ;
    while ( n > 0 )
    {
        citire (x) ;
        sum = sum + x ;
        n = n - 1 ;
    }

    🖨️ ( sum ) ;
}
"""

dict = program.split()
dict = set(dict)


print(dict)