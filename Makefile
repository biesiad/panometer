TARGET = sketch/sketch

all: compile upload

compile: $(TARGET).ino
	arduino-cli compile --fqbn arduino:avr:nano:cpu=atmega328old ./sketch

upload: $(TARGET).arduino.avr.nano.hex
	arduino-cli upload -p /dev/tty.wchusbserial1410 --fqbn arduino:avr:nano:cpu=atmega328old sketch/

clean:
	rm $(TARGET).arduino.avr.nano.hex
