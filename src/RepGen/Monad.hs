--------------------------------------------------------------------------------
module RepGen.Monad where
--------------------------------------------------------------------------------
import RepGen.Type
import RepGen.Config.Type
import RepGen.State.Type
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Logger
--------------------------------------------------------------------------------

type RGM
  = StateT RGState
  ( LoggingT
    ( ReaderT RGConfig
      ( ExceptT RGError
        IO
      )
    )
  )
