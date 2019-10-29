TARGET = sketch
TTY = $(shell ls /dev/tty.*  | grep -e usbserial)
OBJ = $(TARGET).arduino.avr.nano.hex $(TARGET).arduino.avr.nano.elf
BAUD = 9600

$(OBJ): *.ino
	arduino-cli compile --fqbn arduino:avr:nano:cpu=atmega328old ../sketch

.PHONY: clean tty

upload: $(OBJ)
	arduino-cli upload -p $(TTY) --fqbn arduino:avr:nano:cpu=atmega328 ../sketch

upload_old: $(OBJ)
	arduino-cli upload -p $(TTY) --fqbn arduino:avr:nano:cpu=atmega328old ../sketch

debug:
	arduino-cli compile --build-properties build.extra_flags=-DDEBUG --fqbn arduino:avr:nano:cpu=atmega328old ../sketch

clean:
	rm $(OBJ)

screen:
	screen $(TTY) $(BAUD)

tty:
	ls /dev/tty.*  | grep -e usb
