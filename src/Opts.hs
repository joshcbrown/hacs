module Opts where

import Options.Applicative

data Config = Config
    { gap :: Int
    , pixelString :: String
    , file :: String
    }

caveats :: String
caveats =
    "requires the terminal emulator to support truecolor. for a quick test, \
    \this will mean `echo $TERMCOLOR` should output `truecolor`."

opts :: ParserInfo Config
opts =
    info
        (config <**> helper)
        ( fullDesc
            <> progDesc "print a textual representation of an image in a shell\n"
            <> header "hacs - picture to color script"
            <> footer caveats
        )

config :: Parser Config
config =
    Config
        <$> option
            auto
            ( long "gap"
                <> short 'g'
                <> showDefault
                <> value 0
                <> metavar "INT"
                <> help "width of text to left-pad the outputted image"
            )
        <*> strOption
            ( long "pixel"
                <> short 'p'
                <> value "██"
                <> metavar "2-LENGTH-STRING"
                <> help "2-length string sequence to use as pixel. default is a unicode block."
            )
        <*> argument str (metavar "FILE" <> help "path to image file")
