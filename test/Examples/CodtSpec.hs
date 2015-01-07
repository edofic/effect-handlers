{-# OPTIONS_GHC -F -pgmF ./appendCode.sh -optF test-templates/Examples/GeneralSpec.hs #-}
module Examples.CodtSpec where

import qualified Examples.Codt.Reader as Rd
import qualified Examples.Codt.Exception as Exc
import qualified Examples.Codt.Writer as Wrt
import qualified Examples.Codt.Search as Srch
import qualified Examples.Codt.Combined as Cmb
