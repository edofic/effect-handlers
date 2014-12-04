{-# OPTIONS_GHC -F -pgmF ./appendCode.sh -optF test-templates/Examples/GeneralSpec.hs #-}
module Examples.FreeSpec where

import qualified Examples.Free.Reader as Rd
import qualified Examples.Free.Exception as Exc
import qualified Examples.Free.Writer as Wrt
import qualified Examples.Free.Search as Srch
