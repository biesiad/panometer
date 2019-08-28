#include <EEPROM.h>
#include <Wire.h>
#include "Adafruit_VL6180X.h"

#define RECENT_SAMPLES 3              // number of samples for running average

#define SAMPLE_INDEX_OFFSET 0         // index byte address in EEPROM
#define SAMPLES_OFFSET 8              // samples data address in EEPROM

#define PAUSED_OFFSET 1               // paused byte address in EEPROM
#define SAMPLE_DELAY 100             // 5 sec
#define BUTTON_HOLD_DELAY 2000        // push and hold delay ms
#define BUTTON_PIN 4
#define PLOT_HEIGHT 50
#define DISPLAY_WIDTH 128
#define DISPLAY_HEIGHT 64

Adafruit_VL6180X vl = Adafruit_VL6180X();

struct Recent
{
  byte array[RECENT_SAMPLES];
  byte index;
  byte size;
} recent;

unsigned long lastSampleTime;
boolean paused;
boolean buttonActive;
unsigned long buttonActiveTime;
boolean buttonHoldActive;

void addRecent(struct Recent *recent, byte sample)
{
  recent->array[recent->index] = sample;
  recent->index++;
  if (recent->index == recent->size)
    recent->index = 0;
};

byte average(byte *arr, byte size)
{
  short sum = 0;
  byte count = 0;
  for (byte i = 0; i < size; i++)
  {
    if (arr[i] != NULL)
    {
      sum += arr[i];
      count++;
    }
  }
  return sum / count;
};

void setup()
{
  Serial.begin(115200);
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

  EEPROM.write(SAMPLE_INDEX_OFFSET, 0);
  Serial.println("Loading sample index");
  byte sampleIndex = EEPROM.read(SAMPLE_INDEX_OFFSET);
  Serial.print("Loaded ");
  Serial.println(sampleIndex, DEC);
  Serial.println("OK");

  Serial.println("Loading recent samples");
  recent.index = 0;
  recent.size = RECENT_SAMPLES;

  // load last RECENT_SAMPLES samples from EEPROM
  for (byte n = 0; n < RECENT_SAMPLES && sampleIndex - n > 0; n++)
  {
    addRecent(&recent, EEPROM.read(SAMPLES_OFFSET + sampleIndex - n));
    Serial.print("Loaded ");
    Serial.print(EEPROM.read(SAMPLES_OFFSET + sampleIndex - n));
    Serial.print(" at ");
    Serial.println(sampleIndex - n);
  };
  Serial.println("OK");

  paused = false;
  lastSampleTime = 0;
}

void readButton()
{
  if (digitalRead(BUTTON_PIN) == LOW)
  {
    if (!buttonActive)
    {
      buttonActive = true;
      buttonActiveTime = millis();
    }

    if (buttonActive && (millis() > buttonActiveTime + BUTTON_HOLD_DELAY) && !buttonHoldActive)
    {
      buttonHoldActive = true;
      Serial.println("Resetting");
      EEPROM.write(SAMPLE_INDEX_OFFSET, 0);
    }
  }
  else
  {
    if (buttonActive)
    {
      if (buttonHoldActive)
      {
        buttonHoldActive = false;
      }
      else
      {
        paused = !paused;
        Serial.println(" ");
        Serial.println(paused ? "Pausing" : "Resuming");
      }
      buttonActive = 0;
    }
  }
};

// Reads a sample, calculates the average, and saves to EEPROM
void readSample()
{
  Serial.println("----------");
  Serial.println("Reading sample");
  uint8_t sample = vl.readRange();
  uint8_t status = vl.readRangeStatus();

  if (status != VL6180X_ERROR_NONE) {
    Serial.print("VL6180x Error: ");
    Serial.println(status);
    return;
  }

  byte sampleIndex = EEPROM.read(SAMPLE_INDEX_OFFSET);

  Serial.print("sampleIndex: ");
  Serial.println(sampleIndex);

  Serial.print("sample: ");
  Serial.println(sample);

  addRecent(&recent, sample);
  byte avg = average(recent.array, recent.size);

  Serial.print("average: ");
  Serial.println(avg);

  EEPROM.write(SAMPLES_OFFSET + sampleIndex, avg);
};

void drawSamples()
{
  Serial.print("|");
  for (int i = 0; i <= EEPROM.read(SAMPLE_INDEX_OFFSET); i++)
  {
    Serial.print(EEPROM.read(SAMPLES_OFFSET + i));
    Serial.print(" ");
  }
  Serial.println("|");
}

void loop()
{
  // readButton();

  if (paused)
  {
    if (millis() % 1000 == 0)
    {
      Serial.print("Paused ");
      Serial.println((millis() / 1000) % 2 ? "||" : " ");
      delay(1);
    }
    return;
  }

  // read sample if just started or waited enough
  if (!lastSampleTime || millis() > lastSampleTime + SAMPLE_DELAY)
  {
    // when sample index wrapped, we have 256 samples
    if (EEPROM.read(SAMPLE_INDEX_OFFSET) == 0 && lastSampleTime != 0)
    {
      Serial.println("Memory full. Pause.");
      paused = true;
    }
    else
    {
      readSample();
      drawSamples();
      EEPROM.write(SAMPLE_INDEX_OFFSET, EEPROM.read(SAMPLE_INDEX_OFFSET) + 1);
      lastSampleTime = millis();
    }
    delay(1);
  }

  if (millis() % 1000 == 0)
  {
    Serial.print("Recording: ");
    Serial.println((millis() / 1000) % 2 ? "*" : " ");
    delay(1);
  }
}
