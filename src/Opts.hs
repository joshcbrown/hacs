module Opts where

import Options.Applicative

data Config = Config
    { padding :: Int
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
            ( long "padding"
                <> short 'p'
                <> showDefault
                <> value 0
                <> metavar "INT"
                <> help "width of text to left-pad the outputted image"
            )
        <*> argument str (metavar "FILE" <> help "path to image file")
