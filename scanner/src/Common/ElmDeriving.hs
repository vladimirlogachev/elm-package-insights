{-# LANGUAGE UndecidableInstances #-}

-- |
-- This module is a Special Magic Boilerplate to define ElmType and reduce boilerplate
module Common.ElmDeriving (ElmType) where

import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import GHC.Generics (Rep)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Generics.SOP qualified as SOP
import Language.Elm.Name qualified as Name
import Language.Haskell.To.Elm qualified as E
import Relude

newtype ElmType (name :: Symbol) a
  = ElmType a

instance
  (Generic a, Aeson.GToJSON Aeson.Zero (Rep a)) =>
  Aeson.ToJSON (ElmType name a)
  where
  toJSON (ElmType a) =
    Aeson.genericToJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = dropWhile (== '_')} a

instance
  (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a)) =>
  Aeson.FromJSON (ElmType name a)
  where
  parseJSON =
    fmap ElmType . Aeson.genericParseJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = dropWhile (== '_')}

instance
  (SOP.HasDatatypeInfo a, SOP.All2 E.HasElmType (SOP.Code a), KnownSymbol name) =>
  E.HasElmType (ElmType name a)
  where
  elmDefinition =
    Just
      $ E.deriveElmTypeDefinition @a E.defaultOptions {E.fieldLabelModifier = dropWhile (== '_')}
      $ fromString
      $ symbolVal
      $ SOP.Proxy @name

instance
  (SOP.HasDatatypeInfo a, E.HasElmType a, SOP.All2 (E.HasElmDecoder Aeson.Value) (SOP.Code a), E.HasElmType (ElmType name a), KnownSymbol name) =>
  E.HasElmDecoder Aeson.Value (ElmType name a)
  where
  elmDecoderDefinition =
    Just
      $ E.deriveElmJSONDecoder
        @a
        E.defaultOptions {E.fieldLabelModifier = dropWhile (== '_')}
        Aeson.defaultOptions {Aeson.fieldLabelModifier = dropWhile (== '_')}
      $ Name.Qualified moduleName
      $ lowerName
      <> "Decoder"
    where
      Name.Qualified moduleName name = fromString $ symbolVal $ SOP.Proxy @name
      lowerName = T.toLower (T.take 1 name) <> T.drop 1 name

instance
  (SOP.HasDatatypeInfo a, E.HasElmType a, SOP.All2 (E.HasElmEncoder Aeson.Value) (SOP.Code a), E.HasElmType (ElmType name a), KnownSymbol name) =>
  E.HasElmEncoder Aeson.Value (ElmType name a)
  where
  elmEncoderDefinition =
    Just
      $ E.deriveElmJSONEncoder
        @a
        E.defaultOptions {E.fieldLabelModifier = dropWhile (== '_')}
        Aeson.defaultOptions {Aeson.fieldLabelModifier = dropWhile (== '_')}
      $ Name.Qualified moduleName
      $ lowerName
      <> "Encoder"
    where
      Name.Qualified moduleName name = fromString $ symbolVal $ SOP.Proxy @name
      lowerName = T.toLower (T.take 1 name) <> T.drop 1 name
