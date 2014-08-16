module Main where

import Control.Monad (liftM)
import System.Directory (getCurrentDirectory, doesFileExist)
import System.Environment (getArgs, getProgName)
import System.FilePath ((</>))
import System.Exit (ExitCode(..), exitWith)
import System.Process (system)

type Configuration = [String]
type Name          = String
type Tag           = String
type Command       = String

-- | Exit with provided code if code is failure.
exitOnFailure :: ExitCode -> IO ()
exitOnFailure code@(ExitFailure _) = exitWith code
exitOnFailure _ = return ()

-- | Execute external command, exit on error with same exit code.
cmd :: [String] -> IO ()
cmd command = do
    let strCommand = unwords command
    putStrLn strCommand
    system strCommand >>= exitOnFailure

-- | Read additional flags from file, join lines with spaces.
getConf :: FilePath -> IO String
getConf fileName = do
    pwd <- getCurrentDirectory
    let path = pwd </> fileName
    exist <- doesFileExist path
    if exist
      then getStringFromFile path
      else return ""
    where getStringFromFile :: FilePath -> IO String
          getStringFromFile path = liftM (unwords . lines) (readFile path)

-- | Read all additinoal configuration flags for docker cli.
getConfiguration :: IO Configuration
getConfiguration = do
    ports   <- getConf ".docker-ports"
    volumes <- getConf ".docker-volumes"
    env     <- getConf ".docker-env"
    links   <- getConf ".docker-links"
    return [ports, volumes, env, links]

-- | Read read hosts file and build docker command using this data.
getDockerCommand :: IO Command
getDockerCommand = do
    hosts <- getConf ".docker-hosts"
    return $ unwords ["docker", hosts]

-- | Run phony command, apply additional documentation.
make :: Command -> Name -> Tag -> Configuration -> Command -> IO ()
make docker _ tag _ "build"         = cmd [docker, "build -t", tag, "."]
make docker _ tag _ "push"          = cmd [docker, "push", tag]
make docker _ tag _ "pull"          = cmd [docker, "pull", tag]
make docker name _ _ "kill"         = cmd [docker, "kill", name]
make docker name _ _ "rm"           = cmd [docker, "rm", name]
make docker name _ _ "logs"         = cmd [docker, "logs", name]
make docker name _ _ "tailf"        = cmd [docker, "logs -f", name]
make docker _ tag conf "dev"        = cmd $ [docker, "run -t -i"] ++ conf ++ [tag]
make docker _ tag conf "shell"      = cmd $ [docker, "run -t -i"] ++ conf ++ [tag, "bash"]
make docker name tag conf "start"   = cmd $ [docker, "run -d"] ++ ["--name", name] ++ conf ++ [tag]
make docker name tag conf "restart" = make docker name tag conf "kill"
                                   >> make docker name tag conf "rm"
                                   >> make docker name tag conf "start"

make _ _ _ _ command = error $ "Unknown command " ++ command

-- | Get arguments and command name. Use command name as container name.
main :: IO ()
main = do
    args   <- getArgs
    name   <- getProgName
    conf   <- getConfiguration
    docker <- getDockerCommand
    let tag = "gonzih/" ++ name
    mapM_ (make docker name tag conf) args
