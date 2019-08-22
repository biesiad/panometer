#!/usr/bin/python

import Adafruit_DHT
sensor = Adafruit_DHT.DHT11
pin = 22 
humidity, temperature = Adafruit_DHT.read_retry(sensor, pin)
print('{0:0.1f},{1:0.1f}'.format(temperature, humidity))
