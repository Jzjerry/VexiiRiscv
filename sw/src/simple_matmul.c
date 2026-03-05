#include<stdint.h>

#define N 16
#define M 16
#define K 16

int8_t A[N][K];
int8_t B[K][M];
int32_t C[N][M];


int main(){

    for (int i = 0; i < N; i++) {
        for (int j = 0; j < K; j++) {
            A[i][j] = (i + j) % 128; // Initialize A with some values
        }
    }

    for (int i = 0; i < K; i++) {
        for (int j = 0; j < M; j++) {
            B[i][j] = (i * j) % 128; // Initialize B with some values
        }
    }

    for (int i = 0; i < N; i++) {
        for (int j = 0; j < M; j++) {
            C[i][j] = 0; // Initialize C to zero
            for (int k = 0; k < K; k++) {
                C[i][j] += A[i][k] * B[k][j]; // Perform matrix multiplication
            }
        }
    }

    return 0;
}