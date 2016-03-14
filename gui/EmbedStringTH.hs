{-# Language TemplateHaskell #-}
module EmbedStringTH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

embedString :: FilePath -> Q (TExp String)
embedString path =
  do xs <- runIO (readFile path)
     addDependentFile path
     [|| xs ||]
