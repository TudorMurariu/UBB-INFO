public class Coloane {
    int N, n, p;
    int[][] mat;
    int[][] convMat;

    public Coloane(int N, int n, int p, int[][] mat, int[][] convMat) {
        this.N = N;
        this.n = n;
        this.p = p;
        this.mat = mat;
        this.convMat = convMat;
    }

    int[][] newMat;
    public int[][] run() throws InterruptedException {
        newMat = new int[N][N];
        Thread[] threads = new Thread[p];

        int startCol = 0;
        int endCol = -1;
        int rowsPerThread = N / p;
        int remainingCols = N % p;


        for(int k=0;k<p;++k) {
            endCol = startCol + rowsPerThread;
            if (remainingCols > 0) {
                endCol++;
                remainingCols--;
            }

            threads[k] = new MyThread(startCol, endCol);
            threads[k].start();
            startCol = endCol;
        }

        for(int i=0;i<p;++i)
            threads[i].join();

        for(int i = 0;i < N;++i) {
            System.out.println();
            for(int j = 0;j < N;++j)
                System.out.print(newMat[i][j] + " ");
        }
        return newMat;
    }

    class MyThread extends Thread {
        final private int start, end;

        public MyThread(int start, int end) {
            this.start = start;
            this.end = end;
        }

        public void run() {
            for(int j = start;j < end;++j)
                for(int i = 0;i < N;++i) {
                    int sum = 0;
                    for(int i1 = 0;i1 < n;++i1)
                        for(int j1 = 0;j1 < n;++j1)
                            if(i - n/2 + i1 >= 0 && j - n/2 + j1 >= 0 && i - n/2 + i1 < N && j - n/2 + j1 < N)
                                sum += mat[i - n/2 + i1][j - n/2 + j1] * convMat[i1][j1];
                    newMat[i][j] = sum;
                }
        }
    }
}
