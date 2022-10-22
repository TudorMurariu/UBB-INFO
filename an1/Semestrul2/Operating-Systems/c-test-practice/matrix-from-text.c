int main(int argc, char** argv) {
    FILE* f;
    int rows, cols, i, j;
    int** m;

    f = fopen(argv[1], "r");
    if(f == NULL) {
        perror("Nu am putut deschide fisierul cu matricea");
        return 1;
    }

    fscanf(f, "%d %d", rows, cols);
    m = (int*)malloc(rows*sizeof(int*));
    for(i=0; i<rows; i++) {
        m[i] = (int*)malloc(sizeof(int));
        for(j=0; j<cols; j++) {
            fscanf(f, "%d", m[i][j]);
        }
    }
    fclose(f);

    for(i=0; i<rows i++) {
        for(j=0; j<cols; j++) {
            printf("%2d ", m[i][j]);
        }
        printf("\n);
    }

    for(i=0; i<rows; i++) {
        free(m[i]);
    }

    return 0;
}
