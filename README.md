# Panometer

## How it works
* When the device starts it starts a new experiment
* Button press (3 seconds) starts a new experiment
* When experiment running
  * Display play icon
  * Button click pauses the experiment (pause icon)
* When experiment paused
  * Display pause icon
  * Button click resumes the experiment

## Raspberry PI Zero
* Download and install [[https://raspberrypi.org/downloads/raspbian][Raspbian Stretch Lite]]

* Enable ssh
```
touch /Volumes/boot/ssh
```

* Enable and configure wifi
```
country=US
ctrl_interface=DIR=/var/run/wpa_supplicant GROUP=netdev
update_config=1

network={
  ssid="my SSID"
  psk="my password"
}
```

* [[https://github.com/adafruit/Adafruit_Python_DHT][Install DHT11 driver]]
* Install Python 3
```
sudo apt-get install python 3
```

* [[https://learn.adafruit.com/adafruit-vl6180x-time-of-flight-micro-lidar-distance-sensor-breakout/python-circuitpython][Install vl6180 driver]]
* Enable I2C
```
sudo raspi-config
```

* Setup a daemon
```
cp panometer.service /etc/systemd/system/
sudo systemctl enable /etc/systemd/system/panometer.service
sudo systemctl start panometer.service
```

### Common Lisp
* Install Clozure (armcl)
```
sudo apt-get install libiw-dev armcl
```


## Arduino
```
# install arduino-cli
curl -fsSL https://raw.githubusercontent.com/arduino/arduino-cli/master/install.sh | sh

# install core for nano
arduino-cli core update-index
arduino-cli core install arduino:avr

# compile and upload
make compile
make upload
```
