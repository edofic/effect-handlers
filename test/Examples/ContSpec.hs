{-# OPTIONS_GHC -F -pgmF ./appendCode.sh -optF test-templates/Examples/GeneralSpec.hs #-}
module Examples.ContSpec where

import qualified Examples.Cont.Reader as Rd
import qualified Examples.Cont.Exception as Exc
import qualified Examples.Cont.Writer as Wrt
import qualified Examples.Cont.Search as Srch
import qualified Examples.Cont.Combined as Cmb
