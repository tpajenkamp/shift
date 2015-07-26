{-# LANGUAGE TemplateHaskell, CPP #-}
module LensNaming(makeLensPrefixLenses) where

import Control.Lens
import Data.Char
import Language.Haskell.TH


#if MIN_VERSION_lens(4,5,0)
-- | Creates lenses by prepending @"lens"@ to the upper-cased fields.
makeLensPrefixLenses :: Name -> DecsQ
makeLensPrefixLenses = makeLensesWith $ lensRules
    & lensField .~ \_ _ name -> case nameBase name of
                                     (n:ns) -> [TopName (mkName $ "lens" ++ toUpper n : ns)]
                                     _ -> []
#elif MIN_VERSION_lens(4,4,0)
-- | Creates lenses by prepending @"lens"@ to the upper-cased fields.
makeLensPrefixLenses :: Name -> DecsQ
makeLensPrefixLenses = makeLensesWith $ lensRules
    & lensField .~ \_ name -> case nameBase name of
                                     (n:ns) -> [TopName (mkName $ "lens" ++ toUpper n : ns)]
                                     _ -> []
#else
-- | Creates lenses by prepending @"lens"@ to the upper-cased fields.
makeLensPrefixLenses :: Name -> DecsQ
makeLensPrefixLenses = makeLensesWith $ lensRules
    & lensField .~ \name -> case name of
                                 (n:ns) -> Just ("lens" ++ toUpper n : ns))
                                 _ -> Nothing
#endif