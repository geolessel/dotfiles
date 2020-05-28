#!/bin/sh

# Start a new libopencm3 based project for Blue Pill STM32F103C8 board.
# Takes project name as a parameter.

PREFIX="  -->"

EXAMPLES_MASTER=\
"https://raw.githubusercontent.com/geolessel/libopencm3-examples/stlink-flash"

mkdir "$1"
cd "$1"

echo "$PREFIX Initializing git and adding libopencm3 submodule..."
git init .
git submodule add https://github.com/libopencm3/libopencm3

cat << EOF > ".gitignore"
*.a
*.bin
*.d
*.elf
*.hex
*.list
*.map
*.o
*.srec
EOF

echo "$PREFIX Making libopencm3 (this could take a few minutes)..."
cd libopencm3
make 1> /dev/null
cd ..

echo "$PREFIX Downloading required example files..."
wget --no-verbose "${EXAMPLES_MASTER}/examples/rules.mk" -O libopencm3.rules.mk

wget --no-verbose "${EXAMPLES_MASTER}/examples/stm32/f1/Makefile.include" \
  -O libopencm3.target.mk

sed -i '' 's|include ../../../../rules.mk|include ../libopencm3.rules.mk|g' \
  libopencm3.target.mk

echo "$PREFIX Creating Makefile..."
cat << EOF > "Makefile"
BINARY = src/$1

OPENCM3_DIR=libopencm3
LDSCRIPT = \$(OPENCM3_DIR)/lib/stm32/f1/stm32f103x8.ld

include ./libopencm3.target.mk
EOF

echo "$PREFIX Creating blinky example file (src/$1.c)..."
mkdir src
cd src

cat << EOF > "$1.c"
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/gpio.h>

int main(void)
{
    int i;
    rcc_periph_clock_enable(RCC_GPIOC);
    gpio_set_mode(GPIOC, GPIO_MODE_OUTPUT_2_MHZ,
        GPIO_CNF_OUTPUT_PUSHPULL, GPIO13);

    /* Blink the LED (PC13) on the board. */
    while (1) {
        gpio_toggle(GPIOC, GPIO13);
        for (i = 0; i < 800000; i++)
            __asm__("nop");
    }
    return 0;
}
EOF
cd ..

echo "$PREFIX Creating simple README file..."
cat << EOF > "README.md"
# STM32 Project: $1

The main source file is located at \`src/$1.c\`.

## Building

To build the project, run

\`\`\`shell
make
\`\`\`

from the root of the project. After building, if you need a .hex file,
you can run

\`\`\`shell
arm-none-eabi-objcopy -O ihex $1.elf $1.hex
\`\`\`

To upload the resulting build onto the STM32 using an ST-LINK, run

\`\`\`shell
make stflash
\`\`\`
EOF

echo "$PREFIX Done!"
echo
cat README.md
