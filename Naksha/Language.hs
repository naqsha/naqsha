{-# LANGUAGE OverloadedStrings #-}
module Naksha.Language
       ( Language
       , english, french, hindi, malayalam
       ) where

import Data.Text          as T

-- | The language code use to distinguish names in different languages.
newtype Language = Language { unLang :: Text } deriving (Eq, Ord)

lang :: Text -> Language
lang = Language

instance Show Language where
  show = T.unpack . unLang


------------ Some language ----------------------------

-- | The english language
english :: Language
english = lang "en"

-- | The french language
french :: Language
french = lang "fr"

-- | The Hindi language
hindi :: Language
hindi = lang "hin"

-- | The malayalam language
malayalam :: Language
malayalam = lang "ml"
