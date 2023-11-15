#include <iostream>
#include <fstream>
#include <barrier>
#include <string>
#include <chrono>
#include <thread>
using namespace std;

int p;
int n, m, k;

int** f;
int** c;

void read_input_file(const string& filename) {
    ifstream fin(filename);
    fin >> n >> m;
    f = new int* [n];
    for (int i = 0; i < n; ++i)
        f[i] = new int[m];
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            fin >> f[i][j];
        }
    }
    fin >> k >> k;
    c = new int* [k];
    for (int i = 0; i < k; ++i)
        c[i] = new int[k];
    for (int i = 0; i < k; i++) {
        for (int j = 0; j < k; j++) {
            fin >> c[i][j];
        }
    }
}

void write_to_file() {
    ofstream fout("C:\\Users\\tudor\\OneDrive\\Desktop\\L2\\output2.txt");
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            fout << f[i][j] << " ";
        }
        fout << '\n';
    }
}

void deallocateMemory(int**& arr, int rows) {
    for (int i = 0; i < rows; i++) {
        delete[] arr[i];
    }
    delete[] arr;
    arr = nullptr;
}

void aplica_convolutie(int i, int j, int end, int* prev_line, int* curr_line, int* next_line) {
    int s = 0;
    int diff = (k - 1) / 2;
    for (int x = 0; x < k; x++) {
        for (int y = 0; y < k; y++) {
            int ii = i - diff + x;
            int jj = j - diff + y;
            if (ii < 0) {
                ii = 0;
            }
            if (ii >= n) {
                ii = n - 1;
            }
            if (jj < 0) {
                jj = 0;
            }
            if (jj >= m) {
                jj = m - 1;
            }

            int elem_cov;
            if (ii < i) {
                elem_cov = prev_line[jj];
            }
            else if (ii == i) {
                elem_cov = curr_line[jj];
            }
            else if (ii >= end) {
                elem_cov = next_line[jj];
            }
            else {
                elem_cov = f[ii][jj];
            }
            s += elem_cov * c[x][y];
        }
    }
    f[i][j] = s;
}

void run_sequential() {
    int* previous_line = new int[m];
    int* curr_line = new int[m];
    int* next_line = new int[m];

    for (int i = 0; i < m; i++) {
        previous_line[i] = f[0][i];
        curr_line[i] = f[0][i];
        next_line[i] = f[n - 1][i];

    }
    for (int i = 0; i < n; i++) {
        if (i != 0) {
            for (int j = 0; j < m; j++) {
                previous_line[j] = curr_line[j];
                curr_line[j] = f[i][j];
            }
        }
        for (int j = 0; j < m; j++) {
            aplica_convolutie(i, j, n, previous_line, curr_line, next_line);
        }
    }
}

void verify_results() {
    int** thread_result = f;
    run_sequential();
    int** seq_result = f;
    for (int i = 0; i < n; ++i)
    {
        for (int j = 0; j < m; ++j)
        {
            if (thread_result[i][j] != seq_result[i][j])
            {
                std::cerr << "The result is not correct!" << endl;
                return;
            }
        }
    }
}

void rows_thread(int start, int end, barrier<_No_completion_function>& my_barrier) {
    int* previous_line = new int[m];
    int* curr_line = new int[m];
    int* next_line = new int[m];

    int previous_line_number = start - 1;
    int next_line_number = end;
    if (previous_line_number < 0) {
        previous_line_number = 0;
    }
    if (next_line_number >= n) {
        next_line_number = n - 1;
    }

    for (int i = 0; i < m; i++) {
        previous_line[i] = f[previous_line_number][i];
        curr_line[i] = f[previous_line_number][i];
        next_line[i] = f[next_line_number][i];
    }

    my_barrier.arrive_and_wait();

    for (int i = start; i < end; i++)
    {
        if (i != previous_line_number) {
            for (int j = 0; j < m; j++)
            {
                previous_line[j] = curr_line[j];
                curr_line[j] = f[i][j];
            }
        }

        for (int j = 0; j < m; j++) {
            aplica_convolutie(i, j, end, previous_line, curr_line, next_line);
        }
    }
}

void run_with_rows() {
    thread* threads = new thread[p];
    int cat = n / p;
    int rest = n % p;
    int start = 0;
    int end;
    barrier my_barrier{ p };
    for (int i = 0; i < p; i++) {
        int currentNoRows = cat;
        if (rest > 0) {
            rest--;
            currentNoRows++;
        }
        end = start + currentNoRows;
        threads[i] = thread(rows_thread, start, end, ref(my_barrier));
        start += currentNoRows;
    }
    for (int i = 0; i < p; i++)
    {
        threads[i].join();
    }
}

int main(int argc, char* argv[])
{
    //p = stoi(argv[1]);
    //const int file_number = stoi(argv[2]);
    //const int run_option = stoi(argv[3]);
    //const int check_result = stoi(argv[4]);
    //const string filename = "input" + to_string(file_number) + ".txt";

    p = stoi("1");
    const int file_number = stoi("2");
    const int run_option = stoi("0");
    const int check_result = stoi("1");
    const string filename = "C:\\Users\\tudor\\OneDrive\\Desktop\\L2\\input" + to_string(file_number) + ".txt";

    read_input_file(filename);

    const auto start = std::chrono::high_resolution_clock::now();
    switch (run_option)
    {
    case 0:
        run_sequential();
        break;
    case 1:
        run_with_rows();
        break;
    default:
        std::cerr << "Invalid run option." << std::endl;
        return 0;
    }

    const auto end = std::chrono::high_resolution_clock::now();
    const std::chrono::duration<double, std::milli> duration = end - start;
    std::cout << duration.count();

    if (check_result == 1)
    {
        verify_results();
        write_to_file();
    }

    deallocateMemory(f, n);
    deallocateMemory(c, k);
}
