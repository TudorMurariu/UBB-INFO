public class P3 {

    /*
     * Să se determine produsul scalar a doi vectori rari 
     * care conțin numere reale. Un vector este rar atunci când conține multe 
     * elemente nule. Vectorii pot avea oricâte dimensiuni. 
     * De ex. produsul scalar a 2 vectori unisimensionali 
     * [1,0,2,0,3] și [1,2,0,3,1] este 4.
     */
    public static void main(String[] args) throws Exception {
        test();
    }

    public static void test() throws Exception {
        int[] a = new int[]{1,0,2,0,3};
        int[] b = new int[]{1,2,0,3,1};
        System.out.println(vecotrP(a, b));
        a = new int[]{1};
        b = new int[]{1, 2};
    }

    public static int vecotrP(int[] a, int[] b) throws Exception {
        if(a.length != b.length)
            throw new Exception("The Two arrays must have the same length");
        int p = 0;
        for(int i = 0;i < a.length; ++i)
            p += a[i] * b[i];
        return p;
    }
}
