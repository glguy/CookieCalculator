{-# Language TypeOperators, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
module AutoBuilder (autoloadFromBuilder) where

import qualified GI.Gtk as Gtk
import qualified Data.Text as Text
import           GHC.Generics
import           Data.Coerce
import           Control.Exception

data AutoloaderFailure
  = AutoloaderNoSuchObject String
  | AutoloaderBadCast      String
  deriving Show

instance Exception AutoloaderFailure

autoloadFromBuilder ::
  (GLoadBuilder (Rep a), Generic a) => Gtk.Builder -> IO a
autoloadFromBuilder b = to <$> gloadFromBuilder b

class GLoadBuilder f where
  gloadFromBuilder :: Gtk.Builder -> IO (f p)

instance GLoadBuilder f => GLoadBuilder (D1 c f) where
  gloadFromBuilder b = M1 <$> gloadFromBuilder b

instance GLoadBuilder f => GLoadBuilder (C1 c f) where
  gloadFromBuilder b = M1 <$> gloadFromBuilder b

instance GLoadBuilder U1 where
  gloadFromBuilder _ = return U1

instance (GLoadBuilder f, GLoadBuilder g) => GLoadBuilder (f :*: g) where
  gloadFromBuilder b = (:*:) <$> gloadFromBuilder b <*> gloadFromBuilder b

instance (Gtk.ManagedPtrNewtype o, Gtk.GObject o, Selector s) =>
  GLoadBuilder (S1 s (K1 i o)) where

  gloadFromBuilder builder =
     do let name   = selName (M1 Nothing :: S1 s Maybe ())
            orFail m err = maybe (throwIO err) return =<< m
        o <- Gtk.builderGetObject builder (Text.pack name)
                `orFail` AutoloaderNoSuchObject name
        p <- Gtk.castTo coerce o
                `orFail` AutoloaderBadCast name
        return (M1 (K1 p))
