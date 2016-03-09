module AesonTH where

import           Data.Aeson
import qualified Data.ByteString as B
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

loadAeson :: FromJSON a => FilePath -> Q a
loadAeson path =
  do addDependentFile path
     str <- runIO (B.readFile path)
     case eitherDecodeStrict' str of
       Right v -> return v
       Left  e -> fail e

liftResult :: Monad m => Result a -> m a
liftResult (Error   e) = fail e
liftResult (Success x) = return x
