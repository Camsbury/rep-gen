module RepGen.Monad where



import RepGen.Type
import RepGen.Config.Type
import RepGen.State.Type
import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT)
import Control.Monad.Logger (LoggingT)

type RGM
  = StateT RGState
  ( ReaderT RGConfig
    ( ExceptT RGError
      ( LoggingT IO
      )
    )
  )
