TARGET = sketch
TTY = $(shell ls /dev/tty.*  | grep -e usbserial)


all: compile upload screen

compile: $(TARGET).ino
	arduino-cli compile --fqbn arduino:avr:nano:cpu=atmega328old ../sketch

upload: $(TARGET).arduino.avr.nano.hex
	arduino-cli upload -p $(TTY) --fqbn arduino:avr:nano:cpu=atmega328old ../sketch

screen:
	screen $(TTY) 115200

clean:
	rm $(TARGET).arduino.avr.nano.hex
