#include<cstdio>
#include<mpi.h>
#include<cstdlib>
#include<iostream>
#include<fstream>
#include<chrono>
using namespace std;

int n;
int m;
int k;
int lastUsed;
string file_path;

int** matrix;
int** conv_matrix;
int* upCache;
int* downCache;

void printVector(int v[], int n) {
    for (int i = 0; i < n; ++i) {
        cout << v[i] << ' ';
    }
    cout << endl;
}

void update_position(const int x, const int y, int upCache[], const int downCache[], int& lastUsed, bool useDownCache)
{
    int sum = 0;
    const int middle = 1;
    for (int j = 0; j < 3; j++) {
        int jj = y - middle + j;

        jj = max(jj, 0);
        jj = min(jj, m - 1);

        sum += conv_matrix[0][j] * upCache[jj];
    }

    sum += conv_matrix[1][0] * lastUsed;

    for (int j = 1; j < 3; j++) {
        int jj = y - middle + j;

        jj = min(jj, m - 1);

        sum += conv_matrix[1][j] * matrix[x][jj];
    }

    if (useDownCache) {
        for (int j = 0; j < 3; j++) {
            int jj = y - middle + j;

            jj = max(jj, 0);
            jj = min(jj, m - 1);

            sum += conv_matrix[2][j] * downCache[jj];
        }
    }
    else {
        for (int j = 0; j < 3; j++) {
            int jj = y - middle + j;

            jj = max(jj, 0);
            jj = min(jj, m - 1);

            sum += conv_matrix[2][j] * matrix[x + 1][jj];
        }
    }

    if (y > 0) {
        upCache[y - 1] = lastUsed;
    }

    lastUsed = matrix[x][y];

    matrix[x][y] = sum;
}

void read_matrix(int** arr, const int rows, const int columns, ifstream& fin)
{
    for (int i = 0; i < rows; i++)
    {
        for (int j = 0; j < columns; j++)
        {
            fin >> arr[i][j];
        }
    }
}

void read_from_file(const string& file_path)
{
    ifstream fin(file_path);

    fin >> n;
    fin >> m;

    matrix = new int* [n];
    for (int i = 0; i < n; ++i)
        matrix[i] = new int[m];

    read_matrix(matrix, n, m, fin);

    fin >> k;

    conv_matrix = new int* [k];
    for (int i = 0; i < k; ++i)
        conv_matrix[i] = new int[k];

    read_matrix(conv_matrix, k, k, fin);

    MPI_Bcast(&k, 1, MPI_INT, 0, MPI_COMM_WORLD);

    for (int i = 0; i < k; i++) {
        MPI_Bcast(conv_matrix[i], k, MPI_INT, 0, MPI_COMM_WORLD);
    }
}

int main(int argc, char* argv[])
{
    int myid, numprocs, namelen;
    char processor_name[MPI_MAX_PROCESSOR_NAME];

    MPI_Init(NULL, NULL);
    MPI_Comm_rank(MPI_COMM_WORLD, &myid);  // get current process id
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);      // get number of processeser
    MPI_Get_processor_name(processor_name, &namelen);

    MPI_Status status;

    const int file_number = stoi(argv[1]);

    file_path = "date" + to_string(file_number) + ".txt";

    if (myid == 0) {

        read_from_file(file_path);

        const auto time_start = std::chrono::high_resolution_clock::now();


        int rows_per_thread = n / (numprocs - 1);
        int start = 0;
        int end;

        MPI_Bcast(&rows_per_thread, 1, MPI_INT, 0, MPI_COMM_WORLD);
        MPI_Bcast(&m, 1, MPI_INT, 0, MPI_COMM_WORLD);

        for (int i = 1; i < numprocs; i++)
        {
            end = start + rows_per_thread;

            int upCacheIndex = start - 1;
            int downCacheIndex = end;
            upCacheIndex = max(0, upCacheIndex);
            downCacheIndex = min(n - 1, downCacheIndex);

            MPI_Send(matrix[upCacheIndex], m, MPI_INT, i, 0, MPI_COMM_WORLD);

            for (int j = start; j < end; j++) {
                MPI_Send(matrix[j], m, MPI_INT, i, 0, MPI_COMM_WORLD);
            }

            MPI_Send(matrix[downCacheIndex], m, MPI_INT, i, 0, MPI_COMM_WORLD);

            start += rows_per_thread;
        }


        for (int i = 1; i < numprocs; ++i) {
            for (int j = (i - 1) * rows_per_thread; j < i * rows_per_thread; j++) {
                MPI_Recv(matrix[j], m, MPI_INT, i, 0, MPI_COMM_WORLD, &status);
                printVector(matrix[j], m);
            }
        }

        const auto time_end = std::chrono::high_resolution_clock::now();
        const std::chrono::duration<double, std::milli> duration = time_end - time_start;
        std::cout << duration.count() << '\n';

        for (int i = 0; i < n; ++i)
        {
            delete matrix[i];
        }
        delete matrix;

        for (int i = 0; i < k; ++i)
        {
            delete conv_matrix[i];
        }
        delete conv_matrix;

    }
    else {
        MPI_Bcast(&k, 1, MPI_INT, 0, MPI_COMM_WORLD);

        conv_matrix = new int* [k];
        for (int i = 0; i < k; ++i)
            conv_matrix[i] = new int[k];


        for (int i = 0; i < k; i++) {
            MPI_Bcast(conv_matrix[i], k, MPI_INT, 0, MPI_COMM_WORLD);
        }

        MPI_Bcast(&n, 1, MPI_INT, 0, MPI_COMM_WORLD);
        MPI_Bcast(&m, 1, MPI_INT, 0, MPI_COMM_WORLD);

        upCache = new int[m];
        MPI_Recv(upCache, m, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);

        matrix = new int* [n];
        for (int i = 0; i < n; ++i)
            matrix[i] = new int[m];
        for (int i = 0; i < n; i++) {
            MPI_Recv(matrix[i], m, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);
        }

        downCache = new int[m];
        MPI_Recv(downCache, m, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);

        bool useDownCache = false;

        for (int i = 0; i < n; i++)
        {
            lastUsed = matrix[i][0];
            if (i == n - 1)
            {
                useDownCache = true;
            }
            for (int j = 0; j < m; j++)
            {
                update_position(i, j, upCache, downCache, lastUsed, useDownCache);
            }
            upCache[m - 1] = lastUsed;
        }

        //send matrix
        for (int i = 0; i < n; i++)
        {
            MPI_Send(matrix[i], m, MPI_INT, 0, 0, MPI_COMM_WORLD);
        }

        for (int i = 0; i < n; ++i)
        {
            delete matrix[i];
        }
        delete matrix;

        for (int i = 0; i < k; ++i)
        {
            delete conv_matrix[i];
        }
        delete conv_matrix;
        delete upCache;
        delete downCache;
    }


    MPI_Finalize();
}

