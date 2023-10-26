#include <iostream>
#include <thread>
#include <chrono>
#include <fstream>
using namespace std;

int N, n, p;
int **mat, **convMat;
int **newMat;

void readInput() {
    ifstream inputFile("date.txt");
    if (!inputFile) {
        cerr << "Error opening file." << endl;
        return;
    }

    inputFile >> N;
    mat = new int*[N];
    newMat = new int*[N];

    for (int i = 0; i < N; ++i) {
        mat[i] = new int[N];
        newMat[i] = new int[N];
        for (int j = 0; j < N; ++j) {
            inputFile >> mat[i][j];
        }
    }

    inputFile >> n;
    convMat = new int*[n];

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
    int **newMat = new int*[N];
    for (int i = 0; i < N; ++i) {
        newMat[i] = new int[N];
        for (int j = 0; j < N; ++j) {
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
        for (int j = 0; j < N; ++j) {
            cout << newMat[i][j] << " ";
        }
        cout << endl;
    }
}

class ThreadLinii {
private:
    int start, stop;

public:
    ThreadLinii(int start, int stop) : start(start), stop(stop) {}

    void operator()() {
        for (int i = start; i < stop; ++i) {
            for (int j = 0; j < N; ++j) {
                int sum = 0;
                for (int i1 = 0; i1 < n; ++i1) {
                    for (int j1 = 0; j1 < n; ++j1) {
                        if (i - n / 2 + i1 >= 0 && j - n / 2 + j1 >= 0 &&
                            i - n / 2 + i1 < N && j - n / 2 + j1 < N) {
                            sum += mat[i - n / 2 + i1][j - n / 2 + j1] * convMat[i1][j1];
                        }
                    }
                }
                newMat[i][j] = sum;
            }
        }
    }
};

void linii() {
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

    // Print the new matrix
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            cout << newMat[i][j] << " ";
        }
        cout << endl;
    }
}




class ThreadColoane {
private:
    int start, stop;

public:
    ThreadColoane(int start, int stop) : start(start), stop(stop) {}

    void operator()()
    {
        for(int j = start;j < stop;++j)
                for(int i = 0;i < N;++i)
                {
                    int sum = 0;
                    for (int i1 = 0; i1 < n; ++i1) {
                        for (int j1 = 0; j1 < n; ++j1) {
                            if (i - n / 2 + i1 >= 0 && j - n / 2 + j1 >= 0 &&
                                i - n / 2 + i1 < N && j - n / 2 + j1 < N) {
                                sum += mat[i - n / 2 + i1][j - n / 2 + j1] * convMat[i1][j1];
                            }
                        }
                    }
                    newMat[i][j] = sum;
                }
    }
};

void coloane() {
    int startCol = 0;
    int endCol = -1;
    int rowsPerThread = N / p;
    int remainingCols = N % p;

    thread threads[p];

    for (int k = 0; k < p; ++k) {
        endCol = startCol + rowsPerThread;
        if (remainingCols > 0) {
            endCol++;
            remainingCols--;
        }

        threads[k] = thread(ThreadColoane(startCol, endCol));
        startCol = endCol;
    }

    for (int i = 0; i < p; ++i)
        threads[i].join();

    // Print the new matrix
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            cout << newMat[i][j] << " ";
        }
        cout << endl;
    }
}


class ThreadVectorizare {
private:
    int start, stop;

public:
    ThreadVectorizare(int start, int stop) : start(start), stop(stop) {}

    void operator()()
    {
        for (int index = start; index < stop; index++) {
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
};

void vectorizare() {
    int totalElements = N * N;
    int elementsPerThread = totalElements / p;
    int rest = totalElements % p;
    int restCompleted = 0;

    thread threads[p];

    for(int k=0;k<p;++k) {
        int startIndex = k * elementsPerThread + restCompleted;
        int endIndex = (k == p - 1) ? totalElements : (k + 1) * elementsPerThread;

        if(rest != 0) {
            --rest;
            ++endIndex;
            ++restCompleted;
        }


        threads[k] = thread(ThreadVectorizare(startIndex, endIndex));
    }

    for (int i = 0; i < p; ++i)
        threads[i].join();

    // Print the new matrix
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            cout << newMat[i][j] << " ";
        }
        cout << endl;
    }
}

int main()
{
    auto start = chrono::high_resolution_clock::now();

    readInput();
    //secvential();
    //linii();
    //coloane();
    vectorizare();

    auto stop = chrono::high_resolution_clock::now();
    chrono::duration<double, std::micro> duration = stop - start;
    double microseconds = duration.count();
    cout << microseconds / 1000000 << endl;
    return 0;
}
