import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Main {

    static int N, n, p;
    static int[][] mat;
    static int[][] convMat;
// Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass
    public static void main(String[] args) throws FileNotFoundException, InterruptedException {
        readInput();

        System.out.println(N);

        long startTime = System.currentTimeMillis();

//        Secvential p1 = new Secvential(N, n, p, mat, convMat);
//        p1.run();

        Linii p2 = new Linii(N, n, p, mat, convMat);
        p2.run();

//        Coloane p3 = new Coloane(N, n, p, mat, convMat);
//        p3.run();

//        Vectorizare p4 = new Vectorizare(N, n, p, mat, convMat);
//        p4.run();

        long endTime = System.currentTimeMillis();
        System.out.println();
        System.out.println((double)(endTime - startTime)/1E6);//ms
    }

    private static void readInput() throws FileNotFoundException {
        Scanner scanner = new Scanner(new File("date.txt"));
        N = scanner.nextInt();
        mat = new int[N][N];
        for(int i = 0;i < N;++i)
            for(int j = 0;j < N;++j)
                mat[i][j] = scanner.nextInt();

        n = scanner.nextInt();
        convMat = new int[N][N];
        for(int i = 0;i < n;++i)
            for(int j = 0;j < n;++j)
                convMat[i][j] = scanner.nextInt();

        p = scanner.nextInt();
    }
}