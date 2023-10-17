public class Vectorizare {
    int N, n, p;
    int[][] mat;
    int[][] convMat;

    public Vectorizare(int N, int n, int p, int[][] mat, int[][] convMat) {
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

        int totalElements = N * N;
        int elementsPerThread = totalElements / p;

        for(int k=0;k<p;++k) {
            int startIndex = k * elementsPerThread;
            int endIndex = (k == p - 1) ? totalElements : (k + 1) * elementsPerThread;


            threads[k] = new MyThread(startIndex, endIndex);
            threads[k].start();
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
            for (int index = start; index < end; index++) {
                int i = index / N;
                int j = index % N;
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
