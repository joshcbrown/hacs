module MyLib (appMain) where

import Codec.Picture hiding (imagePixels)
import Codec.Picture.STBIR (defaultOptions, resize)
import Control.Applicative
import Control.Monad
import Data.Colour.SRGB
import Data.List
import Options.Applicative (execParser)
import Opts
import System.Console.ANSI
import System.Console.ANSI.Codes
import System.IO (IOMode (WriteMode), hPutStr, withFile)

imagePixels :: Image PixelRGBA8 -> [[PixelRGBA8]]
imagePixels i = [[pixelAt i x y | x <- [0 .. imageWidth i - 1]] | y <- [0 .. imageHeight i - 1]]

ansiSetColor :: PixelRGBA8 -> IO ()
ansiSetColor (PixelRGBA8 r g b _) = setSGR [SetRGBColor Foreground (sRGB24 r g b)]

ansiColorString :: PixelRGBA8 -> String
ansiColorString (PixelRGBA8 r g b _) = "\\x1b[38;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"

resizeImgWidth :: Int -> Image PixelRGBA8 -> Image PixelRGBA8
resizeImgWidth w img = resize defaultOptions w (imageHeight img * w `div` imageWidth img) img

duplicate :: (a -> a) -> [[a]] -> [[a]]
duplicate f xs = [x ++ (f <$> x) | x <- xs]

preprocess :: Bool -> Maybe Int -> Image PixelRGBA8 -> [[PixelRGBA8]]
preprocess invert width = f . imagePixels . maybe id resizeImgWidth width
  where
    f =
        if invert
            then duplicate (\(PixelRGBA8 r g b a) -> PixelRGBA8 (255 - r) (255 - g) (255 - b) a)
            else id

printPicture ::
    (PixelRGBA8 -> IO ()) ->
    (String -> IO ()) ->
    String ->
    Config ->
    [[PixelRGBA8]] ->
    IO ()
printPicture setColor printText newline conf pixels = do
    forM_ pixels $ \row -> do
        printText $ replicate (gap conf) ' '
        forM_ (group row) $ \pixelGroup -> do
            let col = head pixelGroup
                transparent = pixelOpacity col == 0
                groupString = if transparent then "  " else pixelString conf
            unless transparent $ setColor col
            printText $ (concat . replicate (length pixelGroup)) groupString
        printText newline

printPicture' :: Config -> [[PixelRGBA8]] -> IO ()
printPicture' conf pixels = do
    case saveScript conf of
        Just fpath ->
            withFile
                fpath
                WriteMode
                ( \h -> do
                    let putStr' = hPutStr h
                    putStr' "printf \""
                    printPicture (putStr' . ansiColorString) putStr' "\\n" conf pixels
                    putStr' "\""
                )
        Nothing -> printPicture ansiSetColor putStr "\n" conf pixels

appMain :: IO ()
appMain = do
    conf <- execParser opts
    img <- readImage $ file conf
    either print (printPicture' conf . preprocess (invert conf) (width conf) . convertRGBA8) img
