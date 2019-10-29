#include <stdio.h>
#include <math.h>

void nearest_neighbor (unsigned char, unsigned char[], unsigned char, unsigned char[]);
void linearResample (unsigned char, unsigned char[], unsigned char, unsigned char[]);

int main ()
{
  const unsigned char inputc = 10;
  unsigned char input[] = {1, 3, 6, 4, 3, 2, 2, 1, 1, 0};

  for (int i = 0; i < inputc; i++) {
    printf("%i ", input[i]);
  }
  printf("\n");

  const unsigned char outputc = 30;
  unsigned char output[outputc];

  printf("nearest neighbor:        ");
  nearest_neighbor(inputc, input, outputc, output);
  for (int i = 0; i < outputc; i++) {
    printf("%i ", output[i]);
  }
  printf("\n");

  printf("linear:                  ");
  linearResample(inputc, input, outputc, output);
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
  linearResample(inputc, input, outputc, output);
  for (int i = 0; i < outputc; i++) {
    printf("%i ", output[i]);
  }
  printf("\n");

  printf("resample markings short: ");
  unsigned char at = 0;
  unsigned char count = 0;
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
