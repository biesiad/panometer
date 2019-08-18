#include <EEPROM.h>

#define RECENT_SAMPLES 5          // number of samples for running average
#define MAX_SAMPLES 256           // 42h for 1 evert 10 minutes interval
#define INDEX_OFFSET 0            // index byte address in EEPROM
#define SAMPLES_OFFSET 8          // samples data address in EEPROM
#define PAUSED_OFFSET 1           // paused byte address in EEPROM
// #define SAMPLE_DELAY 1000*60*10   // 1 every 10 minutes
#define SAMPLE_DELAY 1000
#define BUTTON_HOLD_DELAY 2000   // push and hold delay ms
#define BUTTON_PIN 4

struct Recent
{
    byte array[RECENT_SAMPLES];
    byte index;
    byte size;
} recent;

byte index;
unsigned long lastSampleTime;
boolean paused;
boolean buttonActive;
unsigned long buttonActiveTime;
boolean buttonHoldActive;

void print_array(uint8_t *array, uint8_t count)
{
    for (int i = 0; i < count; i++)
    {
        Serial.print(array[i]);
        Serial.print("\t");
    }
    Serial.println();
}

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

void setup() {
  Serial.begin(9600);
  Serial.println("Initialize serial");

  recent.index = 0;
  recent.size = RECENT_SAMPLES; 

//  EEPROM.write(0, index);

  index = EEPROM.read(INDEX_OFFSET);
  paused = true;

  // initialize recent with last 5 saved samples 
  byte n = index;
  while (n--)
  {
    add_recent(&recent, EEPROM.read(SAMPLES_OFFSET + index - n));
  };

  buttonActive = false;
  buttonHoldActive = false;
  lastSampleTime = 0;
  
  pinMode(BUTTON_PIN, INPUT);
}

void loop() {  
  if (digitalRead(BUTTON_PIN) == LOW) {
    if (!buttonActive) {
      buttonActive = true;
      buttonActiveTime = millis();     
    }
  
    if (buttonActive && (millis() > buttonActiveTime + BUTTON_HOLD_DELAY) && !buttonHoldActive) {      
      buttonHoldActive = true;
      EEPROM.write(INDEX_OFFSET, index);
      index = 0;
      Serial.println("reset");
    }
  } else {
    if (buttonActive) {   
      if (buttonHoldActive) {      
        buttonHoldActive = false;  
      } else {
        paused = !paused;      
        Serial.print("paused: ");
        Serial.println(paused ? "true" : "false");
      }
      buttonActive = 0;
    }
  }
  
  if (!paused && millis() > lastSampleTime + SAMPLE_DELAY) {   
    byte sample = random(256);

    Serial.print("Index: ");
    Serial.println(index);
  
    Serial.print("Sample: ");
    Serial.println(sample);
  
    add_recent(&recent, sample);
    byte avg = average(recent.array, recent.size);
  
    Serial.print("Average: ");
    Serial.println(avg);
  
    EEPROM.write(SAMPLES_OFFSET + index, avg);
    
    if (index == (MAX_SAMPLES - 1)) {
      index = 0;
    } else {
      index++;
    }
    EEPROM.write(INDEX_OFFSET, index);
     
    Serial.println("----------");  
    lastSampleTime = millis();    
  }
}
