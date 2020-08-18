#!/bin/sh

# Start a new project for Blue Pill STM32F103C8 board.
# Takes project name as a parameter.

if [ $# -eq 0 ] # the number of input arguments is 0
then
    echo "Enter the name of the project as the only argument."
    echo "This will be used for the directory name and in the Makefile."
    exit 1
fi

PREFIX="  -->"

echo "$PREFIX Pulling template files..."
git clone --depth 1 --branch v2 git@github.com:geolessel/stm32-template.git $1
cd $1

echo "$PREFIX Initializing git and adding drivers submodule..."
rm -rf .git
git init .
git submodule add git@github.com:geolessel/stm32f103-drivers.git drivers

cat << EOF > ".gitignore"
bin/
obj/
EOF

echo "$PREFIX Creating simple README file..."
cat << EOF > "README.md"
# STM32 Project: $1

The main source file is located at \`src/$1.c\`.

## Building

To build the project, run

\`\`\`shell
make
\`\`\`

from the root of the project. All binaries can then be found at \`./bin\`.

To upload the resulting build onto the STM32 using an ST-LINK, run

\`\`\`shell
make stflash
\`\`\`

## Resources

* [ST-LINK tutorial](https://github.com/stlink-org/stlink/blob/develop/doc/tutorial.md)
* [Blue Pill board info](https://stm32-base.org/boards/STM32F103C8T6-Blue-Pill.html)
EOF

echo "$PREFIX Updating template files with project name..."
sed -i '' "s/project_name/$1/g" Makefile
# sed -i '' "s/main\.c/$1\.c/g" src/main.c
# mv src/main.c src/$1.c

echo "$PREFIX Making initial commit..."
git add .
git commit -a -m 'Initial commit'

echo "$PREFIX Done!"
echo
# cat README.md
