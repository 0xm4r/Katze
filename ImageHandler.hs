module ImageHandler where

import Codec.Picture
import Codec.Picture.Types

import Data.Either
import Data.Matrix
import qualified Data.Vector as V

getImageMatrix :: FilePath -> IO ( Matrix Int )
getImageMatrix path = do
    img <- readImage path
    case img f 
        (Left err) -> error err
        (Right x) -> imageToMatrix x
        _ -> error "Wrong color format!"



imageToMatrix :: (Image a) -> Matrix Int
imageToMatrix (ImageRGB8 (Image w h d)) = fromList h w $ map rgb8ToGrey $ V.toList d
    where rgb8ToGrey (PixelRGB8 r g b) = 0.21 * r + 0.72 * g + 0.07 * b
imageToMatrix (ImageYCbCr8 (Image w h d)) = fromList h w $ map yCbCr8ToGrey $ V.toList d
    where yCbCr8ToGrey (PixelYCbCr8 y _ _) = y