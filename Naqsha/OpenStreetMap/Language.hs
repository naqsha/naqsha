{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The language codes to use in Open Street Map. The constructors
-- are not exposed to avoid typos in names.
module Naqsha.OpenStreetMap.Language
       ( Language(..)
       -- * Some pre-defined  languages.
       , english, french, hindi, malayalam
       , inNativeScript
       ) where

import Data.Text           as T
import Data.HashMap.Strict as HM
import Data.Hashable

-- | This type captures various languages supported by Open Street
-- Map. To avoid typos in the language use symbolic names like
-- `malayalam` instead of @`Language` "ml"@. If your language does not
-- have a symbolic name please consider contributing it. Such
-- contributions requires very little knowledge of Haskell.

newtype Language = Language { languageCode :: Text -- ^ The code to be use for the language
                            } deriving (Hashable, Eq, Ord)

instance Show Language where
  show = T.unpack . languageCode

------------ Some language ----------------------------

-- TODO: Add more symbolic names. Also update the list of native
-- names.

-- | The english language
english :: Language
english = Language "en"

-- | The French language
french :: Language
french = Language "fr"

-- | The Hindi language
hindi :: Language
hindi = Language "hin"
-- | The Malayalam language.
malayalam :: Language
malayalam = Language "ml"

-- | The name of the language in the language itself.
nativeNames :: HM.HashMap Language Text
nativeNames = HM.fromList [ (french, "Française")
                          , (english, "English")
                          , (hindi, "हिन्दी")
                          , (malayalam, "മലയാളം")
                          ]

-- | The name of the language in the native script itself. For
-- example, @inNativeScript malayalam@ should give @Just "മലയാളം"@.
inNativeScript :: Language -> Maybe Text
inNativeScript = flip HM.lookup nativeNames
