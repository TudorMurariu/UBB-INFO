#include <iostream>
#include <thread>
#include <chrono>
using namespace std;

int N = 1000;
class MyThread {
private:
    int start, stop;
    int* a;
    int* b;
    int* c;

public:
    MyThread(int start, int stop, int* a, int* b, int* c) : start(start), stop(stop), a(a), b(b), c(c) {}

    void operator()()
    {
        for (int index = start; index < stop; ++index)
            c[index] = a[index] + b[index];
    }
};


void cuThreaduri(int *a, int *b, int *c) {
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

        threads[i] = thread(MyThread(start, stop, a, b, c));
        start = stop;
    }

    for (int i = 0; i < p; ++i)
        threads[i].join();
}


void secvential(int *a, int *b, int *c) {
    for(int i = 0;i < N; ++i)
        c[i] = a[i] + b[i];
}


int main()
{
    int* a= new int[N];
    int* b= new int[N];
    int* c= new int[N];

    auto startTime = chrono::high_resolution_clock::now();

    for(int i = 0;i < N; ++i)
        a[i] = b[i] = i;

    //cuThreaduri(a, b, c);
    secvential(a, b, c);

    for(int i = 0;i < N; ++i)
        cout << c[i] << " ";

    auto stopTime = chrono::high_resolution_clock::now();
    chrono::duration<double, std::micro> duration = stopTime - startTime;
    double microseconds = duration.count();
    cout << endl << microseconds / 1000000;
    return 0;
}
