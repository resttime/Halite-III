#!/bin/sh

# Create system folder if doesn't exit
mkdir -p ~/common-lisp

# Create symbolic link of the starter pack in system directory
if [ ! -e ~/common-lisp/hbot ]
   then
       ln -s $(cd `dirname $0` && pwd) ~/common-lisp/hbot
fi

# Compile executable
sbcl --noinform --eval "(asdf:make :hbot)" --eval "(quit)"

# Clean up symbolic link
if [ -h ~/common-lisp/hbot ]
   then
       rm ~/common-lisp/hbot
fi

# Remove directory if empty
rmdir ~/common-lisp > /dev/null

# Run!
./halite --replay-directory replays/ -vvv --width 32 --height 32 "./hbot" "./hbot"
