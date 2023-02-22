
import Prelude hiding( writeFile )
import Data.ByteString.Lazy( writeFile )
import Codec.Picture( readImage )
import Codec.Picture.Saving( imageToPng )
import System.Environment( getArgs )
import Codec.Picture.Tiff.Internal.NG

main :: IO ()
main = do
  testToPNG "layerdemo.tiff" "layerdemo_layer"

