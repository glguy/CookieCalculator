module Math where

import Foreign.C

floor' :: Double -> Double
floor' = realToFrac . c_floor . realToFrac
{-# INLINE floor' #-}

round' :: Double -> Double
round' = realToFrac . c_round . realToFrac
{-# INLINE round' #-}

ceil' :: Double -> Double
ceil' = realToFrac . c_ceil . realToFrac
{-# INLINE ceil' #-}

log1p :: Double -> Double
log1p = realToFrac . c_log1p . realToFrac
{-# INLINE log1p #-}

foreign import ccall unsafe "math.h floor" c_floor :: CDouble -> CDouble
foreign import ccall unsafe "math.h ceil"  c_ceil  :: CDouble -> CDouble
foreign import ccall unsafe "math.h round" c_round :: CDouble -> CDouble
foreign import ccall unsafe "math.h log1p" c_log1p :: CDouble -> CDouble

