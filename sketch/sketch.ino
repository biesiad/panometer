#include <EEPROM.h>
#include <SPI.h>
#include <Wire.h>
#include "Adafruit_VL6180X.h"
#include <Adafruit_GFX.h>
#include <Adafruit_SSD1306.h>

#define BAUD 9600

#define SAMPLES_COUNT_OFFSET 0        // samples count (16 bit) address in EEPROM
#define SAMPLES_OFFSET 8              // samples data address in EEPROM

#define SAMPLES_PER_HOUR 6
#define SAMPLE_DELAY (10 * 60 * 1000L)

#define DISPLAY_WIDTH 128             // display dimensions
#define DISPLAY_HEIGHT 64

#define GRAPH_WIDTH 128               // graph dimensions
#define GRAPH_HEIGHT 50

#define BUTTON_HOLD_DELAY 2000        // push and hold delay ms
#define BUTTON_PIN 3

#define BUTTON_PUSH 1
#define BUTTON_PUSH_AND_HOLD 2

Adafruit_VL6180X vl = Adafruit_VL6180X();
Adafruit_SSD1306 display(DISPLAY_WIDTH, DISPLAY_HEIGHT, &Wire, -1);

// Generated from an image file with ImageMagick:
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

/*
 * Reads button state and returns:
 * BUTTON_PUSH - button have been pushed and released before BUTTON_HOLD_DELAY
 * BUTTON_PUSH_AND_HOLD - button has been pushed for more than BUTTON_HOLD_DELAY and is still pushed
 * 0 - not pushed
 */
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

/*
 * Takes 5 readings, calculates the average, and saves it in EEPROM.
 * Returns sensor status
 */
uint8_t readSample(uint8_t sampleCount)
{
  Serial.print(F("Reading samples"));

  const uint8_t count = 5;
  uint16_t sum = 0;

  for (uint8_t i = 0; i < count; i++) {
    uint8_t range = vl.readRange();
    uint8_t status = vl.readRangeStatus();

    if (status != VL6180X_ERROR_NONE) {
      Serial.print(F(". VL6180x Error: "));
      Serial.println(status);
      return status;
    }

    sum += range;
  }

  uint8_t sample = round(sum / count);

  Serial.print(F(" "));
  Serial.print(sampleCount);
  Serial.print(F("/"));
  Serial.print(GRAPH_WIDTH);
  Serial.print(F(" value: "));
  Serial.println(sample);

  EEPROM.write(SAMPLES_OFFSET + sampleCount, sample);
  return VL6180X_ERROR_NONE;
};

void drawSamples()
{
  uint16_t sampleCount = 0;
  EEPROM.get(SAMPLES_COUNT_OFFSET, sampleCount);

  uint8_t samples[sampleCount];
  uint8_t resampledSamples[GRAPH_WIDTH];
  uint8_t max = 0;

  for (uint8_t i = 0; i < sampleCount; i++) {
    samples[i] = EEPROM.read(SAMPLES_OFFSET + i);
    if (samples[i] > max) {
      max = samples[i];
    }
  }

  linearResample(sampleCount, samples, GRAPH_WIDTH, resampledSamples);

  // draw graph
  display.fillRect(0, DISPLAY_HEIGHT - GRAPH_HEIGHT, DISPLAY_WIDTH + 1, GRAPH_HEIGHT, BLACK);

  for (uint8_t x = 0; x < GRAPH_WIDTH; x++) {
    uint8_t sample = resampledSamples[x];
    uint8_t y = min(DISPLAY_HEIGHT - 1, DISPLAY_HEIGHT - ((GRAPH_HEIGHT * (max - sample)) / max));
    uint8_t height = max(1, DISPLAY_HEIGHT - y);
    display.fillRect(x, y, 1, height, WHITE);
  }

  // hour markings
  for (uint8_t i = 0; i < sampleCount; i++) {
    samples[i] = i % 6 ? 0 : 1;
  }

  linearResample(sampleCount, samples, GRAPH_WIDTH, resampledSamples);

  uint8_t at = 0;
  uint8_t count = 0;

  for (uint8_t x = 1; x < GRAPH_WIDTH; x++) {
    if (resampledSamples[x - 1] == 0 && resampledSamples[x] == 1) {
      while (resampledSamples[x + (++count)] == 1);
      at = x + count / 2;
    }
    if (x == at) {
      count = 0;
      display.fillRect(x, DISPLAY_HEIGHT - GRAPH_HEIGHT, 1, 1, WHITE);
    }
  }

  display.fillRect(0, 0, 60, 14, BLACK);
  display.setCursor(0, 0);
  display.print((sampleCount - 1) / SAMPLES_PER_HOUR, DEC);
  display.print(F("h"));
  display.print(((sampleCount - 1) % SAMPLES_PER_HOUR) * 10, DEC);
  display.print(F("m"));

  display.display();
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

  pinMode(BUTTON_PIN, INPUT);

  display.setTextColor(WHITE);
  display.setTextSize(1);

  display.drawBitmap(32, 12, splashscreen, 64, 26, WHITE);
  display.setCursor(38, 48);
  display.print(F("PANOMETER"));
  display.display();
  delay(3000);

  display.clearDisplay();
  drawSamples();
}

void loop()
{
  static unsigned long lastSampleTime = 0;
  static boolean paused = true;
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

  if (sampleCount >= GRAPH_WIDTH)
  {
      display.fillRect(60, 0, 68, 14, BLACK);
      display.setCursor(60, 0);
      display.print(F("Memory full"));
      display.display();
      return;
  }

  if (paused)
  {
    if (blinkInterval == 0 || blinkInterval > 300)
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
        // TODO: indicate error on the display
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
