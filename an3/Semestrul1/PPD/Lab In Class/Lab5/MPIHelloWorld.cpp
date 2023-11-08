#include<iostream>
#include<mpi.h> 
#include<stdlib.h> 
using namespace std;

void printVector(int v[], int n) {
	for (int i = 0; i < n; i++) {
		cout << v[i] << " ";
	}
	cout << endl;
}

int main(int argc, char* argv[])
{
	int myid, numprocs, namelen;
	char processor_name[MPI_MAX_PROCESSOR_NAME];

	const int N = 10;
	int a[N], b[N], c[N];
	int start = 0, end = 0;

	MPI_Status status;

	MPI_Init(NULL, NULL);
	MPI_Comm_rank(MPI_COMM_WORLD, &myid);  // get current process id
	MPI_Comm_size(MPI_COMM_WORLD, &numprocs);      // get number of processeser
	MPI_Get_processor_name(processor_name, &namelen);

	int* auxA = new int[N / numprocs];
	int* auxB = new int[N / numprocs];
	int* auxC = new int[N / numprocs];

	if (myid == 0) {
		for (int i = 0; i < N; ++i) {
			a[i] = rand() % 10;
			b[i] = rand() % 10;
		}

		printVector(a, N);
		printVector(b, N);
	}

	MPI_Scatter(a, N / numprocs, MPI_INT, auxA, N / numprocs, MPI_INT, 0, MPI_COMM_WORLD); // sau MPI_Scatterv (implementare diferita)
	MPI_Scatter(b, N / numprocs, MPI_INT, auxB, N / numprocs, MPI_INT, 0, MPI_COMM_WORLD);

	for (int i = 0; i < N / numprocs; ++i)
		auxC[i] = auxA[i] + auxB[i];

	MPI_Gather(auxC, N / numprocs, MPI_INT, c, N / numprocs, MPI_INT, 0, MPI_COMM_WORLD); // sau MPI_Gatherv (implementare diferita)
	
	if(myid == 0)
		printVector(c, N);

	MPI_Finalize();
	return 0;
}

int main2(int argc, char* argv[])
{
	int myid, numprocs, namelen;
	char processor_name[MPI_MAX_PROCESSOR_NAME];

	const int N = 10;
	int a[N], b[N], c[N];
	int start = 0, end = 0;

	MPI_Status status;

	MPI_Init(NULL, NULL);
	MPI_Comm_rank(MPI_COMM_WORLD, &myid);  // get current process id
	MPI_Comm_size(MPI_COMM_WORLD, &numprocs);      // get number of processeser
	MPI_Get_processor_name(processor_name, &namelen);

	printf("%s: Hello world from process %d \n", processor_name, myid);
	if (myid == 0) {
		printf("number of processes: %d\n...", numprocs);

		for (int i = 0; i < N; ++i) {
			a[i] = rand() % 10;
			b[i] = rand() % 10;
		}

		printVector(a, N);
		printVector(b, N);
		cout << endl;

		start = 0;
		end = 0;
		int q = N / (numprocs - 1);
		int r = N % (numprocs - 1);

		for (int p = 1; p < numprocs; ++p) {
			end = start + q + (r > 0);
			--r;

			// send data 
			MPI_Send(&start, 1, MPI_INT, p, 0, MPI_COMM_WORLD);
			MPI_Send(&end, 1, MPI_INT, p, 0, MPI_COMM_WORLD);

			MPI_Send(a + start, (end - start), MPI_INT, p, 0, MPI_COMM_WORLD);
			MPI_Send(b + start, (end - start), MPI_INT, p, 0, MPI_COMM_WORLD);
			start = end;
		}

		for (int p = 1; p < numprocs; ++p) {
			MPI_Recv(&start, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);
			MPI_Recv(&end, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);
			MPI_Recv(c + start, (end - start), MPI_INT, p, 0, MPI_COMM_WORLD, &status);
			cout << "Master a primit de la: " << p << endl;
		}

		printVector(c, N);
	}
	else {
		MPI_Recv(&start, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);
		MPI_Recv(&end, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);


		MPI_Recv(a, (end - start), MPI_INT, 0, 0, MPI_COMM_WORLD, &status);
		MPI_Recv(b, (end - start), MPI_INT, 0, 0, MPI_COMM_WORLD, &status);

		for (int i = start; i < end; ++i)
			c[i] = a[i] + b[i];

		MPI_Send(&start, 1, MPI_INT, 0, myid, MPI_COMM_WORLD);
		MPI_Send(&end, 1, MPI_INT, 0, myid, MPI_COMM_WORLD);
		MPI_Send(c + start, (end - start), MPI_INT, 0, myid, MPI_COMM_WORLD);
		cout << "Pentru myid=" << myid << " start, end:" << start << ", " << end << endl;
	}

	MPI_Finalize();
	return 0;
}