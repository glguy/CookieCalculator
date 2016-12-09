{-# LANGUAGE ForeignFunctionInterface #-}
#include "cookies.h"

module CApi where

import CookieClicker
import GameInput
import SaveFormat

import Control.Lens
import Data.Foldable
import Data.Time
import Foreign
import Foreign.C

dbl :: Double -> CDouble
dbl = realToFrac

int :: Int -> CInt
int = fromIntegral

analyzeCookieSave :: CString -> Ptr () -> Ptr Summary -> FunPtr Callback -> IO CInt
analyzeCookieSave savPtr private summary callback =
  do savStr <- peekCString savPtr

     case loadSave savStr of
       Left _ -> return 1
       Right sav ->
         do now <- getCurrentTime

            let i    = saveFileToGameInput now sav
                st   = computeGameState i
                rows = payoff i st
                cps  = computeCps i st

            (#poke struct cookie_summary, cps            ) summary $ dbl cps
            (#poke struct cookie_summary, wrinkler_effect) summary $ dbl $ computeWrinklerEffect i st
            (#poke struct cookie_summary, munched        ) summary $ dbl $ view cookiesMunched i
            (#poke struct cookie_summary, multiplier     ) summary $ dbl $ computeMultiplier i st
            (#poke struct cookie_summary, banked         ) summary $ dbl $ view cookiesBanked i
            (#poke struct cookie_summary, chain6_reserve ) summary $ dbl $ cpsToChainReserve6 cps
            (#poke struct cookie_summary, forfeit        ) summary $ dbl $ view cookiesForfeit i
            (#poke struct cookie_summary, earned         ) summary $ dbl $ view cookiesEarned i
            (#poke struct cookie_summary, prestige       ) summary $ dbl $ view prestigeLevel i
            (#poke struct cookie_summary, heavenly_chips ) summary $ dbl $ view heavenlyChips i
            (#poke struct cookie_summary, wrinklers      ) summary $ int $ view wrinklers i

            for_ rows $ \row ->
              withCString (payoffName row) $ \name ->
                runCallback callback private
                   name
                   (realToFrac (payoffCost row))
                   (realToFrac (payoffDelta row))

            return 0

type Callback = Ptr () -> CString -> CDouble -> CDouble -> IO ()

data Summary

foreign import ccall "dynamic" runCallback :: FunPtr Callback -> Callback

#ifdef C_EXPORTS
foreign export ccall "analyze_cookie_save"
  analyzeCookieSave :: CString -> Ptr () -> Ptr Summary -> FunPtr Callback -> IO CInt
#endif
