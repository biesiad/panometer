#include <EEPROM.h>
// #include <SPI.h>
#include <Wire.h>
#include <Adafruit_GFX.h>
#include <Adafruit_SSD1306.h>

#define RECENT_SAMPLES 5            // number of samples for running average
#define MAX_SAMPLES 256             // 42h for 1 evert 10 minutes interval
#define INDEX_OFFSET 0              // index byte address in EEPROM
#define SAMPLES_OFFSET 8            // samples data address in EEPROM
#define PAUSED_OFFSET 1             // paused byte address in EEPROM
#define SAMPLE_DELAY 1000 * 60 * 10 // 1 every 10 minutes
//#define SAMPLE_DELAY 1000
#define BUTTON_HOLD_DELAY 2000 // push and hold delay ms
#define BUTTON_PIN 4

struct Recent
{
  byte array[RECENT_SAMPLES];
  byte index;
  byte size;
} recent;

byte lastSampleIndex;
unsigned long lastSampleTime;
boolean paused;
boolean buttonActive;
unsigned long buttonActiveTime;
boolean buttonHoldActive;

// Adafruit_SSD1306 display(128, 64, &Wire, -1);

void add_recent(struct Recent *recent, byte sample)
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

  Serial.println("Scanning I2C.");
  for (int i = 1; i < 120; i++) {
    Wire.beginTransmission(i);
    if (Wire.endTransmission() == 0) {
      Serial.print("Found: ");
      Serial.print(i, HEX);
    } else {
      Serial.print(".");
    }
  }
  Serial.println(" OK");

  Serial.print("Initializing button. ");
  pinMode(BUTTON_PIN, INPUT);
  buttonActive = false;
  buttonHoldActive = false;
  Serial.println("OK");

  Serial.println("Initializing VL6180 (sensor).");

  Wire.beginTransmission(0x29);
  if (int res = Wire.endTransmission() != 0)
  {
    Serial.print("VL6180 allocation failed. ERROR");
    for(;;);
  }

  // Serial.println("Initializing SSD1306 (display)");
  // if (!display.begin(SSD1306_SWITCHCAPVCC, 0x3C))
  // {
  //   Serial.print("SSD1306 allocation failed. Exiting.");
  //   for(;;);
  // }

  // display.display();
  return;

  // display.setTextSize(1);
  // display.setTextColor(WHITE);

  // Draw a single pixel in white
  // display.drawPixel(10, 10, WHITE);

  EEPROM.write(0, 0);
  Serial.print("Loading last sample index. ");
  lastSampleIndex = EEPROM.read(INDEX_OFFSET);
  Serial.print("Loaded ");
  Serial.print(lastSampleIndex, DEC);
  Serial.println(". OK");

  Serial.print("Loading recent samples. ");
  recent.index = 0;
  recent.size = RECENT_SAMPLES;

  // load last RECENT_SAMPLES samples from EEPROM
  for (byte n = lastSampleIndex; n < RECENT_SAMPLES; n++)
  {
    add_recent(&recent, EEPROM.read(SAMPLES_OFFSET + lastSampleIndex - n));
  };
  Serial.println("OK");

  Serial.println("Starting");
  paused = true;
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
      EEPROM.write(INDEX_OFFSET, lastSampleIndex);
      lastSampleIndex = 0;
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
        Serial.println(paused ? "Pausing." : "Resuming.");
      }
      buttonActive = 0;
    }
  }
};

void readSample()
{
  Serial.println("----------");
  Serial.println("Reading sample");
  byte sample = random(256);

  Serial.print("lastSampleIndex: ");
  Serial.println(lastSampleIndex);

  Serial.print("sample: ");
  Serial.println(sample);

  add_recent(&recent, sample);
  byte avg = average(recent.array, recent.size);

  Serial.print("average: ");
  Serial.println(avg);

  EEPROM.write(SAMPLES_OFFSET + lastSampleIndex, avg);

  if (lastSampleIndex == (MAX_SAMPLES - 1))
  {
    Serial.println("ERROR: Memory full. Exit.");
    exit(-1);
  }

  EEPROM.write(INDEX_OFFSET, lastSampleIndex);
  Serial.println("----------");
};

void loop()
{
  return;

  readButton();

  if (paused)
  {
    Serial.print("Paused. ");
    Serial.println((millis() / 5000) % 2 ? '||' : ' ');
    // display.drawPixel(10, 10, (millis() / 1000) % 2 ? WHITE : BLACK);
    // display.display();
    return;
  }

  if (millis() > lastSampleTime + SAMPLE_DELAY)
  {
    readSample();
    lastSampleTime = millis();
    // drawSamples();
  }

  if (millis() % 1000 == 0)
  {
    Serial.print("Recording: ");
    Serial.println((millis() / 3000) % 2 ? '*' : ' ');
    if ((millis() / 1000) % 2)
    {
      // int16_t x0, int16_t y0, int16_t r, uint16_t color
      // display.drawCircle(10, 10, );
    }

    // display.display();
  }
}
