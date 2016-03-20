{-# LANGUAGE ForeignFunctionInterface #-}
module CApi where

import Foreign
import Foreign.C

import CookieClicker
import SaveFormat

import Data.Time
import Data.Foldable

analyzeCookieSave :: CString -> Ptr () -> FunPtr Callback -> IO CInt
analyzeCookieSave savPtr private callback =
  do savStr <- peekCString savPtr

     case loadSave savStr of
       Left _ -> return 1
       Right sav ->
         do now <- getCurrentTime

            let i    = saveFileToGameInput now sav
                st   = computeGameState i
                rows = payoff i st

            for_ rows $ \row ->
              withCString (payoffName row) $ \name ->
                runCallback callback private
                   name
                   (realToFrac (payoffCost row))
                   (realToFrac (payoffDelta row))

            return 0


type Callback = Ptr () -> CString -> CDouble -> CDouble -> IO ()

foreign import ccall "dynamic" runCallback :: FunPtr Callback -> Callback
foreign export ccall "analyze_cookie_save" analyzeCookieSave :: CString -> Ptr () -> FunPtr Callback -> IO CInt
