module IdePurescript.PscIdeServer where

import Prelude (const, bind, pure, ($), (++), (==), show)
import Data.Maybe(Maybe(..))
import Data.Either (either)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Aff (Aff, attempt, later', makeAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Par (Par(Par), runPar)
import Control.Alt ((<|>))
import Node.ChildProcess (CHILD_PROCESS, ChildProcess, Exit(Normally), onClose, onError, defaultSpawnOptions, spawn)
import IdePurescript.PscIde (cwd) as PscIde
import PscIde (NET)

data ServerStartResult =
    CorrectPath
  | WrongPath String
  | Started ChildProcess
  | Closed
  | StartError String

startServer :: forall eff. String -> Int -> String -> Aff (cp :: CHILD_PROCESS, console :: CONSOLE, net :: NET, avar :: AVAR | eff) ServerStartResult
startServer exe port rootPath = do
  workingDir <- attempt PscIde.cwd
  either (const launchServer) gotPath workingDir
  where

  launchServer = do
    liftEff $ log "Starting psc-ide-server"
    cp <- liftEff $ spawn exe ["-p", show port] (defaultSpawnOptions { cwd = Just rootPath })
    let handleErr = makeAff $ \_ succ -> do
                      onError cp (\_ -> succ $ StartError "psc-ide-server error")
                      onClose cp (\exit -> case exit of
                        (Normally 0) -> succ Closed
                        (Normally n) -> succ $ StartError $ "Error code returned: "++ show n
                        _ -> succ $ StartError "Other close error"
                      )

    runPar (Par handleErr <|> Par (later' 100 $ pure $ Started cp))

  gotPath workingDir =
    liftEff $ if workingDir == rootPath then
        do
          log $ "Found psc-ide-server with correct path: " ++ workingDir
          pure CorrectPath
      else
        do
          log $ "Found psc-ide-server with wrong path: " ++ workingDir ++ " instead of " ++ rootPath
          pure $ WrongPath workingDir
