#include <stdio.h>
#include <math.h>


void nearest_neighbor (int inputc, int in[], int outputc, int *output)
{
  double ratio = (double)inputc / outputc;

  for (int i = 0; i < outputc; i++) {
    double x = ratio * i;
    double delta_floor = x - floor(x);
    double delta_ceil = (ceil(x) + 1) - x;
    int index = (delta_floor <= delta_ceil) ? floor(x) : ceil(x);

    output[i] = in[index];
  }
}

void linear (int inputc, int input[], int outputc, int *output)
{
  int i = 0;
  double ratio = (double)inputc / outputc;
  double x = (ratio / 2);

  // printf("%f\n", ratio);

  while (x < inputc) {
    double average = 0;

    // x of closest points from the original set
    double xoh = round(x) + 0.5;
    double xol = xoh - 1;

    // deltas between x and closest points
    double dl = x - xol;
    double dh = xoh - x;

    // weights for closest points
    double wl = (1 - dl);
    double wh = (1 - dh);

    // ignore "imaginary" points outside of original set
    if (xol < 0) {
      wl = 0;
      wh = 1;
    }

    if (xoh > inputc) {
      wl = 1;
      wh = 0;
    }

    // weighted average of closest points
    average += input[(int)floor(xol)] * wl;
    average += input[(int)floor(xoh)] * wh;

    output[i] = round(average - 0.1);

    x += ratio;
    i++;
  }
}

int main ()
{
  const int inputc = 10;
  int input[] = {1, 3, 6, 4, 3, 2, 2, 1, 1, 0};

  for (int i = 0; i < inputc; i++) {
    printf("%i ", input[i]);
  }
  printf("\n");

  const int outputc = 30;
  int output[outputc];

  printf("nearest neighbor:        ");
  nearest_neighbor(inputc, input, outputc, output);
  for (int i = 0; i < outputc; i++) {
    printf("%i ", output[i]);
  }
  printf("\n");

  printf("linear:                  ");
  linear(inputc, input, outputc, output);
  for (int i = 0; i < outputc; i++) {
    printf("%i ", output[i]);
  }
  printf("\n");


  for (int i = 0; i < inputc; i++) {
    input[i] = (i + 1) % 7 ? 0 : 1;
    printf("%i ", input[i]);
  }
  printf("\n");

  printf("resample markings:       ");
  linear(inputc, input, outputc, output);
  for (int i = 0; i < outputc; i++) {
    printf("%i ", output[i]);
  }
  printf("\n");

  printf("resample markings short: ");
  int at = 0;
  int count = 0;
  for (int i = 0; i < outputc; i++) {
    if (output[i - 1] == 0 && output[i] == 1) {
      while (output[i + (++count)] == 1);
      at = i + count / 2;
    }
    if (i > 0 && i == at) {
      count = 0;
      printf("%i ", 1);
    } else {
      printf("%i ", 0);
    }
  }
  printf("\n");

  return 0;
}
