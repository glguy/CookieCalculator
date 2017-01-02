{-# Language TypeOperators, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
module AutoBuilder (autoloadFromBuilder) where

import qualified GI.Gtk as Gtk
import qualified Data.Text as Text
import           GHC.Generics

class Gtk.GObject o => GtkThing o where gtkThing :: Gtk.ManagedPtr o -> o
instance GtkThing Gtk.Label  where gtkThing = Gtk.Label
instance GtkThing Gtk.Button where gtkThing = Gtk.Button
instance GtkThing Gtk.Window where gtkThing = Gtk.Window
instance GtkThing Gtk.TreeView where gtkThing = Gtk.TreeView
instance GtkThing Gtk.ListStore where gtkThing = Gtk.ListStore

cast :: (Gtk.GObject o, GtkThing o') => o -> IO (Maybe o')
cast = Gtk.castTo gtkThing

autoloadFromBuilder ::
  (GLoadBuilder (Rep a), Generic a) => Gtk.Builder -> IO a
autoloadFromBuilder b = to <$> gloadFromBuilder b

class GLoadBuilder f where
  gloadFromBuilder :: Gtk.Builder -> IO (f p)

instance GLoadBuilder f => GLoadBuilder (D1 c f) where
  gloadFromBuilder b = M1 <$> gloadFromBuilder b

instance GLoadBuilder f => GLoadBuilder (C1 c f) where
  gloadFromBuilder b = M1 <$> gloadFromBuilder b

instance (GLoadBuilder f, GLoadBuilder g) => GLoadBuilder (f :*: g) where
  gloadFromBuilder b = (:*:) <$> gloadFromBuilder b <*> gloadFromBuilder b

instance (GtkThing o, Selector s) => GLoadBuilder (S1 s (K1 i o)) where
  gloadFromBuilder builder =
     do let name   = selName (M1 Nothing :: S1 s Maybe ())
            mb err = maybe (fail err) return
        o <- mb ("Couldn't find " ++ name)
             =<< Gtk.builderGetObject builder (Text.pack name)
        p <- mb ("Couldn't cast " ++ name) =<< cast o
        return (M1 (K1 p))
