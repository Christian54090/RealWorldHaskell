module SimpleFFI where

import Foreign
import Foreign.C.Types

foreign import ccall "math.h sin"
  c_sin :: CDouble -> CDouble

-- a high-level wrapper
fastsin :: Double -> Double
fastsin x = realToFrac (c_sin (realToFrac x))

simpleFFI_main :: IO ()
simpleFFI_main = mapM_ (print . fastsin) [0/10, 1/10 .. 10/10]
