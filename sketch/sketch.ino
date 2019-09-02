#include <EEPROM.h>
#include <Wire.h>
#include "Adafruit_VL6180X.h"

#define BAUD 9600
#define RECENT_SAMPLES 3              // number of samples for running average

#define SAMPLE_COUNT_OFFSET 0         // index uint8_t address in EEPROM
#define SAMPLES_OFFSET 8              // samples data address in EEPROM
#define SAMPLE_COUNT_MAX 128
#define SAMPLE_DELAY (10*60*1000L)
#define MAX_SAMPLE 70

#define DISPLAY_WIDTH 128
#define DISPLAY_HEIGHT 64

#define GRAPH_WIDTH 128
#define GRAPH_HEIGHT 50

#define BUTTON_HOLD_DELAY 2000        // push and hold delay ms
#define BUTTON_PIN 3

#define BUTTON_PUSH 1
#define BUTTON_PUSH_AND_HOLD 2

Adafruit_VL6180X vl = Adafruit_VL6180X();

struct Recent
{
  uint8_t array[RECENT_SAMPLES];
  uint8_t index;
  uint8_t size;
} recent;

void addRecent(struct Recent *recent, uint8_t sample)
{
  recent->array[recent->index] = sample;
  recent->index++;
  if (recent->index == recent->size)
    recent->index = 0;
};

uint8_t average(uint8_t *arr, uint8_t size)
{
  short sum = 0;
  uint8_t count = 0;
  for (uint8_t i = 0; i < size; i++)
  {
    if (arr[i] != NULL)
    {
      sum += arr[i];
      count++;
    }
  }
  return sum / count;
};

uint8_t readButton()
{
  static boolean buttonActive = false;
  static boolean buttonHoldActive = false;
  static unsigned long buttonActiveTime = 0;

  if (digitalRead(BUTTON_PIN) == HIGH)
  {
    if (!buttonActive)
    {
      buttonActive = true;
      buttonActiveTime = millis();
    }

    if (buttonActive && (millis() > buttonActiveTime + BUTTON_HOLD_DELAY) && !buttonHoldActive)
    {
      buttonHoldActive = true;
      return BUTTON_PUSH_AND_HOLD;
    }
  }
  else
  {
    if (buttonActive)
    {
      buttonActive = 0;
      if (buttonHoldActive)
      {
        buttonHoldActive = false;
      }
      else
      {
        return BUTTON_PUSH;
      }
    }
  }
  return 0;
};

// Reads a sample, calculates the average, and saves to EEPROM
uint8_t readSample(uint8_t sampleCount)
{
  Serial.print("Reading sample");

  uint8_t sample = vl.readRange();
  uint8_t status = vl.readRangeStatus();

  if (status != VL6180X_ERROR_NONE) {
    Serial.print(". VL6180x Error: ");
    Serial.println(status);
    return status;
  }

  addRecent(&recent, sample);
  uint8_t avg = average(recent.array, recent.size);

  Serial.print(" ");
  Serial.print(sampleCount);
  Serial.print("/");
  Serial.print(SAMPLE_COUNT_MAX);
  Serial.print(" value: ");
  Serial.print(sample);
  Serial.print(" average: ");
  Serial.println(avg);

  EEPROM.write(SAMPLES_OFFSET + sampleCount, avg);
  return VL6180X_ERROR_NONE;
};

void drawSamples()
{
  uint16_t sampleCount = 0;
  EEPROM.get(SAMPLE_COUNT_OFFSET, sampleCount);

  uint8_t barWidth = GRAPH_WIDTH / sampleCount;

  Serial.print("|");
  for (int i = 0; i <= sampleCount; i++)
  {
    uint8_t sample = EEPROM.read(SAMPLES_OFFSET + i);
    uint8_t x = i * barWidth;
    uint8_t y = (DISPLAY_HEIGHT - GRAPH_HEIGHT) + ((GRAPH_HEIGHT * (MAX_SAMPLE - sample)) / MAX_SAMPLE);
    uint8_t height = DISPLAY_HEIGHT - y;
    // drawRect(x, y, barWidth, height);

    Serial.print("bar ");
    Serial.print("x: ");
    Serial.print(x);
    Serial.print("y: ");
    Serial.print(x);
    Serial.print("width: ");
    Serial.print(barWidth);
    Serial.print("height: ");
    Serial.println(height);
  }
  Serial.println("|");
}

void setup()
{
  Serial.begin(BAUD);
  Wire.begin();
  Serial.println("Starting");

  Serial.println("Scanning I2C");
  for (int i = 1; i < 120; i++)
  {
    Wire.beginTransmission(i);
    if (Wire.endTransmission() == 0) {
      Serial.print("Found: ");
      Serial.print(i, HEX);
    } else {
      Serial.print(".");
    }
  }
  Serial.println(".");
  Serial.println("OK");

  Serial.println("Initializing VL6180x");
  if (!vl.begin())
  {
    Serial.println("Failed to find VL6180x");
    while (1);
  }
  Serial.println("OK");

  Serial.println("Loading sample count");
  uint16_t sampleCount = 0;
  EEPROM.get(SAMPLE_COUNT_OFFSET, sampleCount);
  Serial.print("Loaded ");
  Serial.println(sampleCount);
  Serial.println("OK");

  Serial.println("Loading recent samples");
  recent.index = 0;
  recent.size = RECENT_SAMPLES;

  // load last RECENT_SAMPLES samples from EEPROM
  for (uint8_t n = 0; n < RECENT_SAMPLES && sampleCount - n > 0; n++)
  {
    addRecent(&recent, EEPROM.read(SAMPLES_OFFSET + sampleCount - n));
    Serial.print("Loaded ");
    Serial.print(EEPROM.read(SAMPLES_OFFSET + sampleCount - n));
    Serial.print(" at ");
    Serial.println(sampleCount - n);
  };
  Serial.println("OK");

  pinMode(BUTTON_PIN, INPUT);
}

void loop()
{
  static unsigned long lastSampleTime = 0;
  static boolean paused = false;

  switch (readButton())
  {
    case BUTTON_PUSH:
      paused = !paused;
      Serial.println(paused ? "Pausing" : "Resuming");
      if (paused) {
        uint16_t sampleCount = 0;
        EEPROM.get(SAMPLE_COUNT_OFFSET, sampleCount);
        Serial.print("Sample count: ");
        Serial.println(sampleCount);
        drawSamples();
      }
      delay(100);
      break;
    case BUTTON_PUSH_AND_HOLD:
      EEPROM.put(SAMPLE_COUNT_OFFSET, 0);
      for (uint8_t i = 0; i < RECENT_SAMPLES; i++)
      {
        recent.array[i] = 0;
      }
      lastSampleTime = 0;
      paused = false;
      Serial.println("Resetting");
      delay(100);
      break;
  }

  if (paused)
  {
    if (millis() % 1000 == 0)
    {
      Serial.print("Paused ");
      Serial.println((millis() / 1000) % 2 ? "||" : " ");
    }
  }
  else
  {
    if (lastSampleTime == 0 || (millis() > (lastSampleTime + SAMPLE_DELAY))) {
      uint16_t sampleCount = 0;
      EEPROM.get(SAMPLE_COUNT_OFFSET, sampleCount);

      if (sampleCount == SAMPLE_COUNT_MAX)
      {
        Serial.println("Memory full. Pause.");
        paused = true;
      }
      else
      {
        if (readSample(sampleCount) == VL6180X_ERROR_NONE) {
          EEPROM.put(SAMPLE_COUNT_OFFSET, sampleCount + 1);
          lastSampleTime = millis();
        } else {
          // if error reading sample, wait 1s and try again
          delay(1000);
        }
      }
    }

    if (millis() % 1000 == 0)
    {
      // Serial.print("Running ");
      // Serial.println((millis() / 1000) % 2 ? "*" : " ");
    }
  }
  delay(1);
}
