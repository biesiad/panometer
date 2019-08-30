TARGET = sketch
TTY = $(shell ls /dev/tty.*  | grep -e usbserial)
OBJ = $(TARGET).arduino.avr.nano.hex

all: $(OBJ) upload screen

$(OBJ): $(TARGET).ino
	arduino-cli compile --fqbn arduino:avr:nano:cpu=atmega328old ../sketch

upload: $(TARGET).arduino.avr.nano.hex
	arduino-cli upload -p $(TTY) --fqbn arduino:avr:nano:cpu=atmega328old ../sketch

screen: $(TARGET).arduino.avr.nano.hex upload
	screen $(TTY) 115200

debug:
	arduino-cli compile --build-properties build.extra_flags=-DDEBUG --fqbn arduino:avr:nano:cpu=atmega328old ../sketch

clean:
	rm $(TARGET).arduino.avr.nano.*
