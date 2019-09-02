TARGET = sketch
TTY = $(shell ls /dev/tty.*  | grep -e usbserial)
OBJ = $(TARGET).arduino.avr.nano.hex $(TARGET).arduino.avr.nano.elf
BAUD = 9600

$(OBJ): $(TARGET).ino
	arduino-cli compile --fqbn arduino:avr:nano:cpu=atmega328old ../sketch

upload: $(TARGET).arduino.avr.nano.hex
	arduino-cli upload -p $(TTY) --fqbn arduino:avr:nano:cpu=atmega328old ../sketch

debug:
	arduino-cli compile --build-properties build.extra_flags=-DDEBUG --fqbn arduino:avr:nano:cpu=atmega328old ../sketch

.PHONY: clean tty

clean:
	rm $(OBJ)

screen:
	screen $(TTY) $(BAUD)

tty:
	ls /dev/tty.*  | grep -e usb
