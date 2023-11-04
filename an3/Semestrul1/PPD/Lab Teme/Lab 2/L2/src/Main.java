import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.Scanner;
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CyclicBarrier;

public class Main {

    static int N, M, n, k;
    static int[][] mat;
    static int[][] convMat;
    static CyclicBarrier barrier;

    public static void main(String[] args) throws FileNotFoundException, InterruptedException {
        readInput();
        barrier = new CyclicBarrier(k);

        Secvential p1 = new Secvential(N, M, n, k, mat, convMat);
        int[][] raspuns = p1.run();

        long startTime = System.currentTimeMillis();
        Thread[] threads = new Thread[k];

        int startRow = 0;
        int endRow = -1;
        int rowsPerThread = N / k;
        int remainingRows = N % k;

        System.out.println();
        for(int p=0;p<k;++p) {
            endRow = startRow + rowsPerThread;
            if (remainingRows > 0) {
                endRow++;
                remainingRows--;
            }

            threads[p] = new MyThread(startRow, endRow);
            threads[p].start();
            startRow = endRow;
        }

        for(int i=0;i<k;++i)
            threads[i].join();

        for(int i = 0;i < N;++i) {
            System.out.println();
            for(int j = 0;j < M;++j)
                System.out.print(mat[i][j] + " ");
        }

        long endTime = System.currentTimeMillis();
        System.out.println();
        System.out.println((double)(endTime - startTime)/1E6);//ms

        System.out.println(Arrays.deepEquals(raspuns, mat));
    }

    private static void readInput() throws FileNotFoundException {
        Scanner scanner = new Scanner(new File("date.txt"));
        N = scanner.nextInt();
        M = scanner.nextInt();
        mat = new int[N][M];
        for(int i = 0;i < N;++i)
            for(int j = 0;j < M;++j)
                mat[i][j] = scanner.nextInt();

        n = scanner.nextInt();
        convMat = new int[n][n];
        for(int i = 0;i < n;++i)
            for(int j = 0;j < n;++j)
                convMat[i][j] = scanner.nextInt();

        k = scanner.nextInt();
    }

    static class MyThread extends Thread {
        final private int start, end;

        public MyThread(int start, int end) {
            this.start = start;
            this.end = end;
        }

        public void run() {

            int[][] arr = new int[2][M];

            if(start > 0)
                for(int j = 0;j < M;++j)
                    arr[0][j] = mat[start - 1][j];

            if(end < N)
                for(int j = 0;j < M;++j)
                    arr[1][j] = mat[end][j];

            int[][] matCopy = new int[end - start][M];
            for(int i = start;i < end;++i)
                for(int j = 0;j < M;++j)
                    matCopy[i - start][j] = mat[i][j];

            try {
//                System.out.println("aaa");
                barrier.await();
//                System.out.println("bbb");
            } catch (InterruptedException | BrokenBarrierException e) {
                throw new RuntimeException(e);
            }

            for(int i = start;i < end;++i)
                for(int j = 0;j < M;++j) {
                    int sum = 0;
                    for(int i1 = 0;i1 < n;++i1)
                        for(int j1 = 0;j1 < n;++j1)
                            if(i - n/2 + i1 >= 0 && j - n/2 + j1 >= 0 && i - n/2 + i1 < N && j - n/2 + j1 < M)
                                if(i - n/2 + i1 < start)
                                    sum += arr[0][j - n/2 + j1] * convMat[i1][j1];
                                else if(i - n/2 + i1 >= end)
                                    sum += arr[1][j - n/2 + j1] * convMat[i1][j1];
                                else
                                    sum += matCopy[i - n/2 + i1 - start][j - n/2 + j1] * convMat[i1][j1];

                    mat[i][j] = sum;
                }
        }
    }
}