module RepGen.Action where

import RepGen.Monad
import RepGen.Action.Type
import RepGen.Action.CalcStats  as CS
import RepGen.Action.TransStats as TS
import RepGen.Action.EnumCands  as EC
import RepGen.Action.EnumResps  as ER
import RepGen.Action.PruneCands as PC
import RepGen.Action.PruneHooks as PH

-- | Runs a given action over state
runAction :: RGAction -> RGM ()
runAction (RGACalcStats  a) = CS.runAction     a
runAction (RGATransStats a) = TS.runAction     a
runAction (RGAEnumCands  a) = EC.runAction     a
runAction (RGAEnumResps  a) = ER.runAction     a
runAction (RGAInitResps  a) = ER.initRunAction a
runAction (RGAPruneCands a) = PC.runAction     a
runAction (RGAPruneHooks a) = PH.runAction     a
