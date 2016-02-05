module IdePurescript.Build where

import Prelude
import Control.Monad.Aff

import Control.Monad.Aff.Class
import Control.Monad.Eff.Class
import Node.ChildProcess as CP
import Data.Maybe

type BuildOptions =
  {
    command :: Command
  , directory :: String
  }

data Command = Command String (Array String)

newtype Error = Error {}
type BuildResult =
  {
    errors :: Array Error
  , success :: Boolean
  }


build :: forall eff. BuildOptions -> Aff (cp :: CP.CHILD_PROCESS | eff) BuildResult
build { command: Command cmd args, directory } = do
  cp <- liftEff $ CP.spawn cmd args (CP.defaultSpawnOptions { cwd = Just directory })
  
  pure {
    errors : [],
    success : false
  }
