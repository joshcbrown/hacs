# hacs
small haskell script to print out an image as best as possible in a terminal. intended for use on startup, e.g. at the end of a `.zshrc`
## usage
```
hacs - picture to color script

Usage: hacs [-g|--gap INT] [-p|--pixel 2-LENGTH-STRING] FILE

  print a textual representation of an image in a shell

Available options:
  -g,--gap INT             width of text to left-pad the outputted image
                           (default: 0)
  -p,--pixel 2-LENGTH-STRING
                           2-length string sequence to use as pixel. default is
                           a unicode block.
  FILE                     path to image file
  -h,--help                Show this help text

requires the terminal emulator to support truecolor. for a quick test, this will
mean `echo $TERMCOLOR` should output `truecolor`.
```
## examples
all using [the example image](drifter.png)

<img width="436" alt="image" src="https://github.com/joshcbrown/hacs/assets/80245312/559292a4-3acf-465f-87a7-0c547392dd46">
<img width="457" alt="image" src="https://github.com/joshcbrown/hacs/assets/80245312/f148971b-ba37-419d-ad7d-1bf64df01e72">


