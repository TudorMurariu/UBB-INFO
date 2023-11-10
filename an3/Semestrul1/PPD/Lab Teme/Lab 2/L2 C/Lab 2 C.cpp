#include <barrier>
#include <iostream>
#include <thread>
#include <chrono>
#include <fstream>
using namespace std;

int N, M, n, p;
int** mat, ** convMat;
int** newMat;

void readInput() {
    ifstream inputFile("date.txt");
    if (!inputFile) {
        cerr << "Error opening file." << endl;
        return;
    }

    inputFile >> N;
    inputFile >> M;
    mat = new int* [N];
    newMat = new int* [N];

    for (int i = 0; i < N; ++i) {
        mat[i] = new int[M];
        newMat[i] = new int[M];
        for (int j = 0; j < M; ++j) {
            inputFile >> mat[i][j];
        }
    }

    inputFile >> n;
    convMat = new int* [n];

    for (int i = 0; i < n; ++i) {
        convMat[i] = new int[n];
        for (int j = 0; j < n; ++j) {
            inputFile >> convMat[i][j];
        }
    }

    inputFile >> p;

    inputFile.close();
}

void secvential() {
    int** newMat = new int* [N];
    for (int i = 0; i < N; ++i) {
        newMat[i] = new int[N];
        for (int j = 0; j < M; ++j) {
            int sum = 0;
            for (int i1 = 0; i1 < n; ++i1) {
                for (int j1 = 0; j1 < n; ++j1) {
                    if (i - n / 2 + i1 >= 0 && j - n / 2 + j1 >= 0 && i - n / 2 + i1 < N && j - n / 2 + j1 < N) {
                        sum += mat[i - n / 2 + i1][j - n / 2 + j1] * convMat[i1][j1];
                    }
                }
            }
            newMat[i][j] = sum;
        }
    }

    // Print the new matrix
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < M; ++j) {
            cout << newMat[i][j] << " ";
        }
        cout << endl;
    }
}

class ThreadLinii {
private:
    int start, stop;

public:
    ThreadLinii(int start, int stop, barrier barrier_) : start(start), stop(stop) {}

    void operator()() {
        int arr[2][M];
        int copyMat[stop - start][M];

        if (start > 0)
            for (int j = 0; j < M; ++j)
                arr[0][j] = mat[start - 1][j];

        if (stop < N)
            for (int j = 0; j < M; ++j)
                arr[1][j] = mat[stop][j];

        for (int i = start; i < stop; ++i)
            for (int j = 0; j < M; ++j)
                matCopy[i - start][j] = mat[i][j];

        barrier.arrive_and_wait();
        for (int i = start; i < stop; ++i) {
            for (int j = 0; j < M; ++j) {
                int sum = 0;
                for (int i1 = 0; i1 < n; ++i1) {
                    for (int j1 = 0; j1 < n; ++j1) {
                        if (i - n / 2 + i1 >= 0 && j - n / 2 + j1 >= 0 && i - n / 2 + i1 < N && j - n / 2 + j1 < M)
                            if (i - n / 2 + i1 < start)
                                sum += arr[0][j - n / 2 + j1] * convMat[i1][j1];
                            else if (i - n / 2 + i1 >= stop)
                                sum += arr[1][j - n / 2 + j1] * convMat[i1][j1];
                            else
                                sum += matCopy[i - n / 2 + i1 - start][j - n / 2 + j1] * convMat[i1][j1];
                    }
                }
                newMat[i][j] = sum;
            }
        }
    }
};

void linii(barrier barrier_) {
    int startRow = 0;
    int endRow = -1;
    int rowsPerThread = N / p;
    int remainingRows = N % p;

    thread threads[p];

    for (int k = 0; k < p; ++k) {
        endRow = startRow + rowsPerThread;
        if (remainingRows > 0) {
            endRow++;
            remainingRows--;
        }

        threads[k] = thread(ThreadLinii(startRow, endRow));
        startRow = endRow;
    }

    for (int i = 0; i < p; ++i)
        threads[i].join();
}

int check(int a[N][M], int b[N][M]) {
    int i, j;
    for (i = 0; i < N; i++)
        for (j = 0; j < M; j++)
            if (a[i][j] != b[i][j])
                return 0;
    return 1;
}

int main()
{
    readInput();
    barrier barrier_{ p };

    secvential();
    auto start = chrono::high_resolution_clock::now();

    linii(barrier_);

    auto stop = chrono::high_resolution_clock::now();
    chrono::duration<double, std::micro> duration = stop - start;
    double microseconds = duration.count();
    cout << check(newMat, mat);

    cout << microseconds / 1000000 << endl;

    return 0;
}
