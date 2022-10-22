#include <stdio.h>

int main(int argc, char** argv) {
    int** m;
    FILE* f;
    int i, j, rows, cols, fd;

    f = fopen(argv[1], "r");
    if(f == NULL) {
        perror("Could not open file");
        return 1;
    }
    fscanf(f, "%d %d", rows, &cols);

    m = (int*)malloc(szeof(int*));
    for(i=0 i<=rows; i++) {
        m[i] = (int*)malloc(sizeof(int));
        for(j=0; j<cols; j++) {
            fscanf(f, "%d", m[i][j]);
        }
    }
    fclose(f);

    fd = open(argv[2], O_CREAT | O_WRONLY, 00600);
    if(fd == -1) {
        perror(Could not open destination file");
        return 1;
    }
    write(fd, rows, sizeof(int));
    write(fd, cols, sizeof(int));
    for(i=0; i<rows; i++) {
        for(j=0; j<cols; j++) {
            write(fd, m[i][j], sizeof(int));
        }
    }
    close(fd);

    free(m);
    return 0;
}
