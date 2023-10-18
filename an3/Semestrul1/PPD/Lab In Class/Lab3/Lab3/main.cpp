#include <iostream>
#include <thread>
#include <chrono>
using namespace std;

const int MAX_N = 1000;
int a[MAX_N], b[MAX_N], c[MAX_N];


int N = 10;


class MyThread {
private:
    int start, stop;

public:
    MyThread(int start, int stop) : start(start), stop(stop) {}

    void operator()()
    {
        for (int index = start; index < stop; ++index)
            c[index] = a[index] + b[index];
    }
};


void cuThreaduri() {
    int p = 4;
    int start = 0;
    int stop = -1;
    int elemsPerThread = N / p;
    int remainingElems = N % p;
    thread threads[p];

    for(int i=0;i<p;++i) {
        stop = start + elemsPerThread;

        if(remainingElems) {
            stop++;
            remainingElems--;
        }

        threads[i] = thread(MyThread(start, stop));
        start = stop;
    }

    for (int i = 0; i < p; ++i)
        threads[i].join();
}

void secvential() {
    int p = 4;
    int start = 0;
    int stop = -1;
    int elemsPerThread = N / p;
    int remainingElems = N % p;

    for(int i = 0;i < N; ++i)
        c[i] = a[i] + b[i];
}

int main()
{
    auto startTime = chrono::high_resolution_clock::now();


    for(int i = 0;i < N; ++i)
        a[i] = b[i] = i;

    cuThreaduri();
    //secvential();


    for(int i = 0;i < N; ++i)
        cout << c[i] << " ";

    auto stopTime = chrono::high_resolution_clock::now();
    chrono::duration<double, std::micro> duration = stopTime - startTime;
    double microseconds = duration.count();
    cout << endl << microseconds / 1000000;
    return 0;
}
