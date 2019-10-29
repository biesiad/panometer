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

void linearResample (uint8_t inputc, uint8_t input[], uint8_t outputc, uint8_t *output)
{
  // https://entropymine.com/imageworsener/resample/
  // https://www.ldv.ei.tum.de/fileadmin/w00bfa/www/content_uploads/Vorlesung_3.4_Resampling.pdf

  int i = 0;
  double ratio = (double)inputc / outputc;
  double x = (ratio / 2);

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

    output[i] = round(average);

    x += ratio;
    i++;
  }
}
