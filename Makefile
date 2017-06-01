SOURCE=main.pl
TARGET=main

all: $(TARGET)

$(TARGET): *.pl
	swipl -o $@ -O -c $(SOURCE)

play: $(TARGET)
	swipl -s main.pl

clean:
	@rm -Rf a.out $(TARGET)
