module Opts where

import Options.Applicative

data Config = Config
    { gap :: Int
    , pixelString :: String
    , file :: String
    }

desc :: String
desc =
    "Print an ASCII representation of an image.\n\
    \Requires the terminal to support truecolor. For a quick test, \
    \this will mean `echo $TERMCOLOR` should output `truecolor`."
opts :: ParserInfo Config
opts =
    info
        (config <**> helper)
        ( fullDesc
            <> progDesc desc
            <> header "ptcs - picture to color script"
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
                <> help "2-length string sequence to use as pixel. default is a unicode block."
            )
        <*> argument str (metavar "FILE" <> help "path to image file")
