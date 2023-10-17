public class Linii {
    int N, n, p;
    int[][] mat;
    int[][] convMat;

    public Linii(int N, int n, int p, int[][] mat, int[][] convMat) {
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

        int startRow = 0;
        int endRow = -1;
        int rowsPerThread = N / p;
        int remainingRows = N % p;


        for(int k=0;k<p;++k) {
            endRow = startRow + rowsPerThread;
            if (remainingRows > 0) {
                endRow++;
                remainingRows--;
            }

            threads[k] = new MyThread(startRow, endRow);
            threads[k].start();
            startRow = endRow;
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
            for(int i = start;i < end;++i)
                for(int j = 0;j < N;++j) {
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
