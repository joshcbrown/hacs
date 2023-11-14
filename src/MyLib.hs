module MyLib (appMain) where

import Codec.Picture hiding (imagePixels)
import Control.Monad
import Data.Colour.SRGB
import Data.List
import Options.Applicative (execParser)
import Opts
import System.Console.ANSI
import System.Console.ANSI.Codes

imagePixels :: Image PixelRGBA8 -> [[PixelRGBA8]]
imagePixels i = [[pixelAt i x y | x <- [0 .. imageWidth i - 1]] | y <- [0 .. imageHeight i - 1]]

juicyToSRGB :: PixelRGBA8 -> Colour Float
juicyToSRGB (PixelRGBA8 r g b _) = sRGB24 r g b

printPicture :: Config -> Image PixelRGBA8 -> IO ()
printPicture conf i = do
    let pixels = imagePixels i

    forM_ pixels $ \row -> do
        putStr $ replicate (gap conf) ' '
        forM_ (group row) $ \pixelGroup -> do
            let rgbCol = head pixelGroup
            let srgbCol = juicyToSRGB rgbCol
            let groupString = if pixelOpacity rgbCol == 0 then "  " else pixelString conf
            when (pixelOpacity rgbCol /= 0) $ setSGR [SetRGBColor Foreground srgbCol]
            putStr $ (concat . replicate (length pixelGroup)) groupString
        putStrLn ""

appMain :: IO ()
appMain = do
    conf <- execParser opts
    img <- readImage $ file conf
    either print (printPicture conf . convertRGBA8) img
