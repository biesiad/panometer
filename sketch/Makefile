TARGET = sketch

all: compile upload screen

compile: $(TARGET).ino
	arduino-cli compile --fqbn arduino:avr:nano:cpu=atmega328old ../sketch

upload: $(TARGET).arduino.avr.nano.hex
	arduino-cli upload -p /dev/tty.usbserial-AH02UGML --fqbn arduino:avr:nano:cpu=atmega328old ../sketch
	# arduino-cli upload -p /dev/tty.wchusbserial1410 --fqbn arduino:avr:nano:cpu=atmega328old ../sketch

screen:
	picocom -b 115200 /dev/tty.usbserial-AH02UGML
	# screen /dev/tty.usbserial-AH02UGML 115200

clean:
	rm $(TARGET).arduino.avr.nano.hex
