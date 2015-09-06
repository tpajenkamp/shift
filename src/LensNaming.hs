{-# LANGUAGE TemplateHaskell, CPP #-}
-----------------------------------------------------------------------------
--
-- Module      :  LensNaming
-- Copyright   :  (c) 2015, Thomas Pajenkamp
-- License     :  BSD3
--
-- Maintainer  :  tpajenka
-- Stability   :
-- Portability :
--
-- | Lens naming scheme for generating 'Control.Lens.Type.Lens'es and 'Control.Lens.Type.Traversal's via @TemplateHaskell@.
--
-- ==== Example
-- The snippet
-- 
-- @
-- data Test = TestA { fieldA :: Int, fieldB :: Char, _fieldC :: String }
--           | TestB { filedA :: Int }
-- $(makeLensPrefixLenses ''Test)
-- @
--
-- will create (note that @_fieldC@ does not start with a letter)
--
-- @
-- lensFieldA :: Lens' Test Int
-- lensFieldB :: Traversal' Test Char
-- @
--
-- ==== See also
-- @'Control.Lens.TH.makeLenses'@
-----------------------------------------------------------------------------

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