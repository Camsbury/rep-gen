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
runAction (RGACalcStats  a) = doRunAction a
runAction (RGATransStats a) = doRunAction a
runAction (RGAEnumCands  a) = doRunAction a
runAction (RGAEnumResps  a) = doRunAction a
runAction (RGAPruneCands a) = doRunAction a
runAction (RGAPruneHooks a) = doRunAction a

class RunAction a where
  doRunAction :: a -> RGM ()

instance RunAction CalcStats where
  doRunAction = CS.runAction

instance RunAction TransStats where
  doRunAction = TS.runAction

instance RunAction EnumCands where
  doRunAction = EC.runAction

instance RunAction EnumResps where
  doRunAction = ER.runAction

instance RunAction PruneCands where
  doRunAction = PC.runAction

instance RunAction PruneHooks where
  doRunAction = PH.runAction

