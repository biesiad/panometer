/*
EEPROM 1024B - persistent
Flash 32KB - sketch
SRAM 2KB (2048B) - variables, stack, heap
*/

#define RECENT_SAMPLES_COUNT 5
#define SAMPLES_COUNT 10

#include <stdio.h>
#include <stdlib.h>

struct Recent
{
    uint8_t array[RECENT_SAMPLES_COUNT];
    uint8_t index;
    uint8_t size;
} recent = {.index = 0, .size = RECENT_SAMPLES_COUNT};

struct Samples
{
    uint8_t array[SAMPLES_COUNT];
    uint8_t removed;
    uint8_t size;
} samples = {.removed = 0, .size = SAMPLES_COUNT, .array = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}};

uint8_t average(uint8_t *arr, uint8_t count)
{
    uint8_t sum = 0;
    uint8_t non_null_count = 0;
    for (int i = 0; i < count; i++)
    {
        if (arr[i] != NULL)
        {
            sum += arr[i];
            non_null_count++;
        }
    }
    return sum / non_null_count;
};

void print_array(uint8_t *array, uint8_t count)
{
    for (int i = 0; i < count; i++)
    {
        printf("%i ", array[i]);
    }
    printf("\n");
}

void add_recent(struct Recent *recent, u_int8_t sample)
{
    recent->array[recent->index] = sample;
    recent->index++;
    if (recent->index == recent->size)
        recent->index = 0;
};

void add_sample(struct Samples *samples, uint8_t sample)
{
    printf("removed: %i, %i\n", samples->removed, samples->array[samples->removed]);

    for (int i = samples->removed; i < samples->size - 1; i++) {
        samples->array[i] = samples->array[i+1];
    }
    samples->array[samples->size - 1] = sample;

    if (samples->removed == samples->size) {
        samples->removed = 0;
        printf("resetting removed\n");
    } else {
        samples->removed += 1;
    }
}

int main(void)
{
    printf("starting\n");
    print_array(samples.array, samples.size);

    for (int i = 1; i <= 20; i++) {
        add_sample(&samples, 100 + i);
        print_array(samples.array, samples.size);
        printf("\n");
    }

    // uint8_t sample;
    // if (sample = read_sample())
    // {
    //     add_recent(&recent, sample);
    //     add_sample(&samples, get_average(recent.array));
    // }

    // add_recent(&recent, 1);
    // printf("average: %i\n", get_average(recent.array));
    // print_array(recent.array, recent.size);

    // add_recent(&recent, 2);
    // printf("average: %i\n", get_average(recent.array));
    // print_array(recent.array, RECENT_SAMPLES_COUNT);

    // add_recent(&recent, 3);
    // printf("average: %i\n", get_average(recent.array));
    // print_array(recent.array, RECENT_SAMPLES_COUNT);

    // add_recent(&recent, 4);
    // printf("average: %i\n", get_average(recent.array));
    // print_array(recent.array, RECENT_SAMPLES_COUNT);

    // add_recent(&recent, 5);
    // printf("average: %i\n", get_average(recent.array));
    // print_array(recent.array, RECENT_SAMPLES_COUNT);

    // uint8_t recent_samples[RECENT_SAMPLES_COUNT];
    // uint8_t samples[SAMPLES_COUNT];

    // FILE *fp = fopen("./experiments/3757606263.csv", "r");

    // if (fp == NULL)
    //     exit(EXIT_FAILURE);

    // char *line = NULL;
    // size_t len = 0;
    // ssize_t read;

    // int count = 0;
    // while (count++ < 10) {
    //     uint8_t sample = rand();

    // while ((read = getline(&line, &len, fp)) != -1) {
    // add to recent 5 samples
    // get the average
    // add average to samples
    // downsample samples if > SAMPLES_COUNT
    // print
    // sleep 1 minute

    // printf("Retrieved line of length %zu:\n", read);
    // printf("%i\n", sample);
    // }

    // if (line)
    //     free(line);

    // fclose(fp);

    exit(EXIT_SUCCESS);
};

// read sample
