#include <EEPROM.h>
#include <Wire.h>

#define RECENT_SAMPLES 3              // number of samples for running average
#define MAX_SAMPLES 256               // 42h for 1 every 10 minutes interval
#define NEXT_SAMPLE_INDEX_OFFSET 0                // index byte address in EEPROM
#define SAMPLES_OFFSET 8              // samples data address in EEPROM
#define PAUSED_OFFSET 1               // paused byte address in EEPROM
#define SAMPLE_DELAY 1000 * 60 * 10   // 10 minutes
#define BUTTON_HOLD_DELAY 2000        // push and hold delay ms
#define BUTTON_PIN 4
#define PLOT_HEIGHT 50
#define DISPLAY_WIDTH 128
#define DISPLAY_HEIGHT 64

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
  for (int i = 1; i < 120; i++) {
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

  Serial.println("Initializing button");
  pinMode(BUTTON_PIN, INPUT);
  buttonActive = false;
  buttonHoldActive = false;
  Serial.println("OK");

  Serial.println("Probing VL6180");
  Wire.beginTransmission(0x29);
  if (int res = Wire.endTransmission() != 0)
  {
    Serial.println("Can't find VL6180 at 0x29");
    for(;;);
  }
  Serial.println("OK");

  Serial.println("Probing SSD1306");
  Wire.beginTransmission(0x29);
  if (int res = Wire.endTransmission() != 0)
  {
    Serial.println("Can't find SSD1306 at 0x3C");
    for(;;);
  }
  Serial.println("OK");

  EEPROM.write(0, 0);
  Serial.println("Loading last sample index");
  byte lastSampleIndex = EEPROM.read(NEXT_SAMPLE_INDEX_OFFSET) - 1;
  Serial.print("Loaded ");
  Serial.println(lastSampleIndex, DEC);
  Serial.println("OK");

  Serial.println("Loading recent samples");
  recent.index = 0;
  recent.size = RECENT_SAMPLES;

  // load last RECENT_SAMPLES samples from EEPROM
  for (byte n = lastSampleIndex; n < RECENT_SAMPLES; n++)
  {
    addRecent(&recent, EEPROM.read(SAMPLES_OFFSET + lastSampleIndex - n));
  };
  Serial.println("OK");

  paused = true;
  lastSampleTime = 0;

  Serial.println("Paused");
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
      EEPROM.write(NEXT_SAMPLE_INDEX_OFFSET, 0);
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

// Reads a sample, calculates the average, and saves to EEPROM
void readSample()
{
  Serial.println("----------");
  Serial.println("Reading sample");
  byte sample = random(256);

  byte sampleIndex = EEPROM.read(NEXT_SAMPLE_INDEX_OFFSET);
  Serial.print("sampleIndex: ");
  Serial.println(sampleIndex);

  Serial.print("sample: ");
  Serial.println(sample);

  addRecent(&recent, sample);
  byte avg = average(recent.array, recent.size);

  Serial.print("average: ");
  Serial.println(avg);

  EEPROM.write(SAMPLES_OFFSET + sampleIndex, avg);

  if (sampleIndex == (MAX_SAMPLES - 1))
  {
    Serial.println("ERROR: Memory full. Exit.");
    exit(-1);
  }

  EEPROM.write(NEXT_SAMPLE_INDEX_OFFSET, sampleIndex + 1);
  Serial.println("----------");
};

void drawSamples()
{
  byte nextSampleIndex = EEPROM.read(NEXT_SAMPLE_INDEX_OFFSET);
  byte boxY = DISPLAY_HEIGHT - PLOT_HEIGHT;
  byte boxW = DISPLAY_WIDTH / nextSampleIndex;
  byte boxX, boxH;

  Serial.println('Samples:');
  for (int i = 0; i < nextSampleIndex; i++)
  {
    boxX = i * boxW;
    boxH = EEPROM.read(SAMPLES_OFFSET + i);
    Serial.print('boxX: ');
    Serial.print(boxX);
    Serial.print(', boxY: ');
    Serial.print(boxY);
    Serial.print(', boxW: ');
    Serial.print(boxW);
    Serial.print(', boxH: ');
    Serial.println(boxH);
  }
}

void loop()
{
  readButton();

  if (paused)
  {
    if (millis() % 1000 == 0)
    {
      Serial.print("Paused ");
      Serial.println((millis() / 1000) % 2 ? '||' : ' ');
    }
    return;
  }

  if (millis() > lastSampleTime + SAMPLE_DELAY)
  {
    readSample();
    lastSampleTime = millis();
    drawSamples();
  }

  if (millis() % 1000 == 0)
  {
    Serial.print("Recording: ");
    Serial.println((millis() / 1000) % 2 ? '*' : ' ');
  }
}
