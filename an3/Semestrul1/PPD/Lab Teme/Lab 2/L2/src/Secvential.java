public class Secvential {
    int N, M, n, p;
    int[][] mat;
    int[][] convMat;

    public Secvential(int N, int M, int n, int p, int[][] mat, int[][] convMat) {
        this.N = N;
        this.M = M;
        this.n = n;
        this.p = p;
        this.mat = mat;
        this.convMat = convMat;
    }

    public int[][] run() {
        int[][] newMat = new int[N][M];
        for(int i = 0;i < N;++i)
            for(int j = 0;j < M;++j) {
                int sum = 0;
                for(int i1 = 0;i1 < n;++i1)
                    for(int j1 = 0;j1 < n;++j1)
                        if(i - n/2 + i1 >= 0 && j - n/2 + j1 >= 0 && i - n/2 + i1 < N && j - n/2 + j1 < M)
                            sum += mat[i - n/2 + i1][j - n/2 + j1] * convMat[i1][j1];
                newMat[i][j] = sum;
            }

        for(int i = 0;i < N;++i) {
            System.out.println();
            for(int j = 0;j < M;++j)
                System.out.print(newMat[i][j] + " ");
        }

        return newMat;
    }
}
