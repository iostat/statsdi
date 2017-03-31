module Control.Monad.Stats.TH where

import qualified Language.Haskell.TH as TH

defineCounter :: String -> [(String, String)] -> TH.DecsQ
defineCounter = undefined

defineGauge :: String -> [(String, String)] -> TH.DecsQ
defineGauge = undefined

defineTimer :: String -> [(String, String)] -> TH.DecsQ
defineTimer = undefined

defineHistogram :: String -> [(String, String)] -> TH.DecsQ
defineHistogram = undefined

defineSet :: String -> [(String, String)] -> TH.DecsQ
defineSet = undefined
