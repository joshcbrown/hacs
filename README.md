# hacs

small haskell script to print out an image as best as possible in a terminal. intended for use on startup, e.g. at the end of a `.zshrc`

## usage

```
hacs - picture to color script

Usage: hacs [-g|--gap INT] [-p|--pixel STRING] [-w|--width INT]
            [-s|--save-script FILEPATH] [-i|--invert] FILEPATH

  print a textual representation of an image in a shell

Available options:
  -g,--gap INT             width of text to left-pad the outputted image.
                           (default: 0)
  -p,--pixel STRING        2-length string sequence to use as pixel. default is
                           a unicode block.
  -w,--width INT           width of output image. it may be necessary for large
                           images to scale down.
  -s,--save-script FILEPATH
                           whether to save a shell script to print the image.
                           executing the shell script will be significantly more
                           performant.
  -i,--invert              duplicate and invert the colours of the image.
  FILEPATH                 path to image file
  -h,--help                Show this help text

requires the terminal emulator to support truecolor. for a quick test, this will
mean `echo $TERMCOLOR` should output `truecolor`.
```

## examples

all using [the example image](drifter.png)

<img width="436" alt="image" src="https://github.com/joshcbrown/hacs/assets/80245312/559292a4-3acf-465f-87a7-0c547392dd46">
<img width="807" alt="image" src="https://github.com/joshcbrown/hacs/assets/80245312/04c96754-e8c8-4229-aec1-4c5fe54c2c39">
