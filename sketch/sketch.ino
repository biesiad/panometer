#include <EEPROM.h>
#include <SPI.h>
#include <Wire.h>
#include "Adafruit_VL6180X.h"
#include <Adafruit_GFX.h>
#include <Adafruit_SSD1306.h>

#define BAUD 9600
#define RECENT_SAMPLES 5              // number of samples for running average

#define SAMPLES_COUNT_OFFSET 0        // samples count (16 bit) address in EEPROM
#define SAMPLE_MAX_OFFSET 2           // container depth (8 bit) address in EEPROM
#define SAMPLES_OFFSET 8              // samples data address in EEPROM
#define SAMPLE_DELAY (10*60*1000L)    // sample every 10 minutes

#define DISPLAY_WIDTH 128
#define DISPLAY_HEIGHT 64
#define GRAPH_WIDTH 128
#define GRAPH_HEIGHT 50

#define BUTTON_HOLD_DELAY 2000        // push and hold delay ms
#define BUTTON_PIN 3

#define BUTTON_PUSH 1
#define BUTTON_PUSH_AND_HOLD 2

Adafruit_VL6180X vl = Adafruit_VL6180X();
Adafruit_SSD1306 display(DISPLAY_WIDTH, DISPLAY_HEIGHT, &Wire, -1);

// Generated from an image with:
//   convert splashscreen.bmp -monochrome -compress none -depth 1 splashscreen.pbm
static const unsigned char PROGMEM splashscreen[] = {
  B00000000, B00000000, B00000000, B00000000, B11111111, B10000111, B11100000, B00000000,
  B00000000, B00000000, B00000000, B11110000, B01111111, B11000111, B11111110, B00000000,
  B00000000, B00000000, B00001111, B11111000, B01111111, B11100011, B11111111, B10000000,
  B00000000, B00000000, B00111111, B11111100, B00111111, B11110011, B11111111, B11100000,
  B00000000, B00000000, B00011111, B11111110, B00111111, B11111001, B11111111, B11110000,
  B00000000, B00001100, B00011111, B11111111, B00011111, B11111101, B11111111, B11111000,
  B00000000, B00111111, B00001111, B11111111, B10011111, B11111111, B11111111, B11111100,
  B00000000, B11111111, B10000111, B11111111, B11011111, B11111111, B11111111, B11111110,
  B00000001, B11111111, B11000111, B11111111, B11111111, B11111111, B11111111, B11111110,
  B00000111, B11111111, B11100011, B11111111, B11111111, B11111111, B11111111, B11111110,
  B00001111, B11111111, B11110011, B11111111, B11111111, B11111111, B11111111, B11111110,
  B00011111, B11111111, B11111001, B11111111, B11111111, B11111111, B11111111, B11111100,
  B00111111, B11111111, B11111101, B11111111, B11111111, B11111111, B11111111, B11111100,
  B01111111, B11111111, B11111111, B11111111, B11111111, B11111111, B11111111, B11111000,
  B11111111, B11111111, B11111111, B11111111, B11111111, B11111111, B11111111, B11110000,
  B11111111, B11111111, B11111111, B11111111, B11111111, B11111111, B11111111, B11100000,
  B11111111, B11111111, B11111111, B11111111, B11111111, B11111111, B11111111, B11000000,
  B11111111, B11111111, B11111111, B11111111, B11111111, B11111111, B11111111, B10000000,
  B11111111, B11111111, B11111111, B11111111, B11111111, B11111111, B11111110, B00000000,
  B11111111, B11111111, B11111111, B11111111, B11111111, B11111111, B11111000, B00000000,
  B01111111, B11111111, B11111111, B11111111, B11111111, B11111111, B11100000, B00000000,
  B00111111, B11111111, B11111111, B11111111, B11111111, B11111111, B10000000, B00000000,
  B00011111, B11111111, B11111111, B11111111, B11111111, B11111100, B00000000, B00000000,
  B00000111, B11111111, B11111111, B11111111, B11111111, B11000000, B00000000, B00000000,
  B00000001, B11111111, B11111111, B11111111, B11111100, B00000000, B00000000, B00000000,
  B00000000, B00011111, B11111111, B11111111, B00000000, B00000000, B00000000, B00000000
};

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

// Reads a sample (mean of 5 readings), calculates running average, and saves to EEPROM
uint8_t readSample(uint8_t sampleCount)
{
  Serial.print(F("Reading samples"));

  uint8_t readingSum = 0;
  const uint8_t readingCount = 5;

  for (int i = 0; i < readingCount; i ++) {
    uint8_t sample = vl.readRange();
    uint8_t status = vl.readRangeStatus();

    if (status != VL6180X_ERROR_NONE) {
      Serial.print(F(". VL6180x Error: "));
      Serial.println(status);
      return status;
    }
    readingSum += sample;
  }

  uint8_t sample = readingSum / readingCount;

  addRecent(&recent, sample);
  uint8_t avg = average(recent.array, recent.size);

  Serial.print(F(" "));
  Serial.print(sampleCount);
  Serial.print(F("/"));
  Serial.print(GRAPH_WIDTH);
  Serial.print(F(" value: "));
  Serial.print(sample);
  Serial.print(F(" average: "));
  Serial.println(avg);

  EEPROM.write(SAMPLES_OFFSET + sampleCount, avg);
  return VL6180X_ERROR_NONE;
};

void drawSamples()
{
  uint16_t sampleCount = 0;
  EEPROM.get(SAMPLES_COUNT_OFFSET, sampleCount);

  uint8_t barWidth = GRAPH_WIDTH / sampleCount;

  display.fillRect(0, DISPLAY_HEIGHT - GRAPH_HEIGHT, DISPLAY_WIDTH + 1, GRAPH_HEIGHT, BLACK);

  for (int i = 0; i < sampleCount; i++)
  {
    uint8_t sample = EEPROM.read(SAMPLES_OFFSET + i);
    uint8_t x = i * barWidth;
    uint8_t y = min(DISPLAY_HEIGHT - 1, DISPLAY_HEIGHT - ((GRAPH_HEIGHT * (EEPROM.read(SAMPLE_MAX_OFFSET) - sample)) / EEPROM.read(SAMPLE_MAX_OFFSET)));
    uint8_t height = max(1, DISPLAY_HEIGHT - y);
    display.fillRect(x, y, barWidth, height, WHITE);
  }

  display.fillRect(0, 0, 60, 14, BLACK);
  display.setCursor(0, 0);
  display.print(((sampleCount - 1) * SAMPLE_DELAY) / (60 * 60 * 1000L) , DEC);
  display.print(F("h"));
  display.print((((sampleCount - 1) * SAMPLE_DELAY) % (60 * 60 * 1000L)) / (60 * 1000L), DEC);
  display.print(F("m"));

  display.display();
}

void calibrate() {
  // load 5 samples and count the average
  uint8_t count = 5;
  short sum = 0;
  for (uint8_t n = 0; n < count; n++)
  {
    uint8_t sample = vl.readRange();
    uint8_t status = vl.readRangeStatus();

    if (status == VL6180X_ERROR_NONE)
    {
      sum += sample;
    } else {
      Serial.print(F("Can't calibrate VL6180X. Error: "));
      Serial.println(status);
      while(1);
    }
  };

  // 255 means it hasn't been callibrated, so max 254
  EEPROM.write(SAMPLE_MAX_OFFSET, min(sum / count, 254));
}

void setup()
{
  Serial.begin(BAUD);
  Wire.begin();
  Serial.println(F("Starting"));

  Serial.println(F("Scanning I2C"));
  for (int i = 1; i < 120; i++)
  {
    Wire.beginTransmission(i);
    if (Wire.endTransmission() == 0) {
      Serial.print(F("Found: "));
      Serial.print(i, HEX);
    } else {
      Serial.print(F("."));
    }
  }
  Serial.println(F("."));
  Serial.println(F("OK"));

  Serial.println(F("Initializing VL6180x"));
  if (!vl.begin())
  {
    Serial.println(F("Failed to find VL6180x"));
    while (1);
  }
  Serial.println(F("OK"));

  Serial.println(F("Initializing SSD1306"));
  // SSD1306_SWITCHCAPVCC = generate display voltage from 3.3V internally
  if(!display.begin(SSD1306_SWITCHCAPVCC, 0x3C)) {
    Serial.println(F("Failed to find SSD1306"));
    while(1);
  }

  display.clearDisplay();

  Serial.println(F("Loading sample count"));
  uint16_t sampleCount = 0;
  EEPROM.get(SAMPLES_COUNT_OFFSET, sampleCount);
  Serial.print(F("Loaded "));
  Serial.println(sampleCount);
  Serial.println(F("OK"));

  Serial.println(F("Loading recent samples"));
  recent.index = 0;
  recent.size = RECENT_SAMPLES;

  // load last RECENT_SAMPLES samples from EEPROM
  for (uint8_t n = 0; n < RECENT_SAMPLES && sampleCount - n > 0; n++)
  {
    addRecent(&recent, EEPROM.read(SAMPLES_OFFSET + sampleCount - n));
    Serial.print(F("Loaded "));
    Serial.print(EEPROM.read(SAMPLES_OFFSET + sampleCount - n));
    Serial.print(F(" at "));
    Serial.println(sampleCount - n);
  };
  Serial.println(F("OK"));

  pinMode(BUTTON_PIN, INPUT);

  display.setTextColor(WHITE);
  display.setTextSize(1);

  display.drawBitmap(32, 12, splashscreen, 64, 26, WHITE);
  display.setCursor(38, 48);
  display.print(F("PANOMETER"));
  display.display();

  // Calibration
  unsigned long buttonPressedStartMillis = millis();
  boolean buttonPressed = digitalRead(BUTTON_PIN) == HIGH;

  if (buttonPressed)
  {
    display.setCursor(120, 0);
    display.print("C");
    display.display();
  }

  while (buttonPressed)
  {
    // if pressed for > BUTTON_HOLD_DELAY, calibrate
    if (millis() - buttonPressedStartMillis > BUTTON_HOLD_DELAY)
    {
      calibrate();
      display.display();
      break;
    }
    buttonPressed = digitalRead(BUTTON_PIN) == HIGH;
  }

  if (EEPROM.read(SAMPLE_MAX_OFFSET) == 255)
  {
    calibrate();
  }

  display.fillRect(115, 0, DISPLAY_WIDTH - 115, 14, BLACK);
  display.setCursor(115, 0);
  display.print(EEPROM.read(SAMPLE_MAX_OFFSET));

  delay(3000);
  display.clearDisplay();

  drawSamples();
}

void loop()
{
  static unsigned long lastSampleTime = 0;
  static boolean paused = false;
  static uint16_t blinkInterval = 0;
  static boolean blink = false;

  switch (readButton())
  {
    case BUTTON_PUSH:
      paused = !paused;
      blinkInterval = 0;
      blink = true;
      Serial.println(paused ? F("Pausing") : F("Resuming"));
      break;
    case BUTTON_PUSH_AND_HOLD:
      EEPROM.put(SAMPLES_COUNT_OFFSET, 0);
      for (uint8_t i = 0; i < RECENT_SAMPLES; i++)
      {
        recent.array[i] = 0;
      }
      lastSampleTime = 0;
      paused = false;
      blinkInterval = 0;
      blink = true;
      Serial.println(F("Resetting"));
      display.fillRect(60, 0, 68, 14, BLACK);
      display.setCursor(60, 0);
      display.print(F("Resetting.."));
      display.display();
      delay(1000);
      display.clearDisplay();
      break;
  }

  uint16_t sampleCount;
  EEPROM.get(SAMPLES_COUNT_OFFSET, sampleCount);

  if (sampleCount == GRAPH_WIDTH)
  {
      display.fillRect(60, 0, 68, 14, BLACK);
      display.setCursor(60, 0);
      display.print(F("Memory full"));
      display.display();
      return;
  }

  if (paused)
  {
    if (blinkInterval == 0 || blinkInterval > 500)
    {
      display.fillRect(DISPLAY_WIDTH - 10, 0, 10, 14, BLACK);
      uint8_t color = blink ? WHITE : BLACK;
      display.fillRect(DISPLAY_WIDTH - 6, 1, 2, 4, color);
      display.fillRect(DISPLAY_WIDTH - 2, 1, 2, 4, color);
      display.display();
      blinkInterval = 0;
      blink = !blink;
      // Serial.print(F("Paused "));
      // Serial.println((millis() / 1000) % 2 ? "||" : " "));
    }
    blinkInterval++;
  }
  else
  {
    if (lastSampleTime == 0 || (millis() > (lastSampleTime + SAMPLE_DELAY))) {
      if (readSample(sampleCount) == VL6180X_ERROR_NONE) {
        EEPROM.put(SAMPLES_COUNT_OFFSET, sampleCount + 1);
        lastSampleTime = millis();
        drawSamples();
      } else {
        // if error reading sample, wait 1s and try again
        delay(1000);
      }
    }

    if (blinkInterval == 0 || blinkInterval > 1000)
    {
      display.fillRect(DISPLAY_WIDTH - 10, 0, 10, 14, BLACK);
      uint8_t color = blink ? WHITE : BLACK;
      display.fillCircle(DISPLAY_WIDTH - 3, 2, 2, color);
      display.display();
      blinkInterval = 0;
      blink = !blink;
      // Serial.print(F("Running "));
      // Serial.println((millis() / 1000) % 2 ? "*" : " "));
    }
    blinkInterval++;
  }
  delay(1);
}
