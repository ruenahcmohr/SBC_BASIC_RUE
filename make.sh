sdasz80 -b -l -g -o foo.rel build.asm
sdcc -mz80 -o card.ihex --no-std-crt0 ./foo.rel
