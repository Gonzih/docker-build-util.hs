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

data DockerArgs = DockerArgs { exe    :: Command
                             , name   :: Name
                             , tag    :: Tag
                             , conf   :: Configuration
                             }

-- TODO: Try to refactor this with pattern guards http://www.haskell.org/haskellwiki/Pattern_guard
-- | Run phony command, apply additional documentation.
make :: DockerArgs -> Command -> IO ()
make da@(DockerArgs { exe  = docker
                    , name = cName
                    , tag  = cTag
                    , conf = dConf
                    })
     subcmd
    | isA tagBased      = cmd [docker, subcmd, cTag]
    | isA nameBased     = cmd [docker, subcmd, cName]
    | subcmd == "build" = cmd [docker, "build -t", cTag, "."]
    | subcmd == "tailf" = cmd [docker, "logs -f", cName]
    | subcmd == "dev"   = cmd $ [docker, "run -t -i"] ++ dConf ++ [cTag]
    | subcmd == "shell" = cmd $ [docker, "run -t -i"] ++ dConf ++ [cTag, "bash"]
    | subcmd == "start" = cmd $ [docker, "run -d", "--name", cName] ++ dConf ++ [cTag]
    | subcmd == "start" = cmd $ [docker, "run -d", "--name", cName] ++ dConf ++ [cTag]
    | subcmd == "restart" = make da "kill" >> make da "rm" >> make da "start"
    | otherwise         = error $ "Unknown command " ++ subcmd
        where isA       = elem subcmd
              tagBased  = ["push", "pull"]
              nameBased = ["kill", "rm", "logs"]

-- | Get arguments and command name. Use command name as container name.
main :: IO ()
main = do
    progArgs   <- getArgs
    progName   <- getProgName
    dockerConf <- getConfiguration
    dockerCmd  <- getDockerCommand
    let dockerTag = "gonzih/" ++ progName
        dockerArgs = DockerArgs { exe  = dockerCmd
                                , name = progName
                                , tag  = dockerTag
                                , conf = dockerConf }
    mapM_ (make dockerArgs) progArgs
