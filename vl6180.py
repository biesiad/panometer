import time

import board
import busio

import adafruit_vl6180x

# Create I2C bus.
i2c = busio.I2C(board.SCL, board.SDA)

# Create sensor instance.
sensor = adafruit_vl6180x.VL6180X(i2c)

# Read the range in millimeters and print it.
range_mm = sensor.range
light_lux = sensor.read_lux(adafruit_vl6180x.ALS_GAIN_1)
print('{0},{1}'.format(range_mm, light_lux))
  
# Read the light, note this requires specifying a gain value:
# - adafruit_vl6180x.ALS_GAIN_1 = 1x
# - adafruit_vl6180x.ALS_GAIN_1_25 = 1.25x
# - adafruit_vl6180x.ALS_GAIN_1_67 = 1.67x
# - adafruit_vl6180x.ALS_GAIN_2_5 = 2.5x
# - adafruit_vl6180x.ALS_GAIN_5 = 5x
# - adafruit_vl6180x.ALS_GAIN_10 = 10x
# - adafruit_vl6180x.ALS_GAIN_20 = 20x
# - adafruit_vl6180x.ALS_GAIN_40 = 40x
