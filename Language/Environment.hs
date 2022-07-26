{-#  LANGUAGE LambdaCase           #-}
{-#  LANGUAGE OverloadedLists      #-}
{-#  LANGUAGE ImportQualifiedPost  #-}


module Language.Environment
( Environment                   -- Environment Type
, createGlobalEnv               -- Environment
, createLocalEnv                -- Environment
, dropEnv                       -- Environment -> Environment
, get                           -- String -> Environment -> Maybe a
, define                        -- String -> a -> Environment -> Environment
, assign                        -- String -> a -> Environment -> (Environment, Bool)
, mkFuncArgs                    -- [(String, a)] -> Environment -> Environment
, runEnvAction
, emptyEnv
, EnvError(..)
) where

import Data.Map.Strict             (Map)
import Data.Map.Strict qualified as Map
import Data.IORef
import Control.Monad.Except

newtype EnvError = UndefinedVar String

type EnvAction a = ExceptT EnvError IO a

runEnvAction :: EnvAction a -> IO (Either EnvError a)
runEnvAction = runExceptT


type Env a = IORef (Map String a)

data Environment a
    = Global (Env a)
    | Local  (Env a) (Environment a)    -- local, parent

createEnv :: IO (Env a)
createEnv = newIORef []

createGlobalEnv :: IO (Environment a)
createGlobalEnv = Global <$> createEnv

createLocalEnv :: Environment a -> IO (Environment a)
createLocalEnv p = do
    e <- createEnv
    pure $ Local e p 

dropEnv :: Environment a -> Environment a
dropEnv = \case
    Global _ -> error "cannot drop global env!"
    Local _ p -> p

getEnv :: Environment a -> Env a
getEnv (Global e) = e
getEnv (Local e p) = e

get :: String -> Environment a -> EnvAction a
get name = \case 
    Global e   -> do
        env <- liftIO $ readIORef e
        case Map.lookup name env of
            Nothing -> throwError (UndefinedVar name)
            Just value -> return value
    Local  e p -> do
        env <- liftIO $ readIORef e
        case Map.lookup name env of 
            Nothing    -> get name p
            Just value -> return value


define :: String -> a -> Environment a -> IO ()
define name value env = modifyIORef' (getEnv env) (Map.insert name value)


mkFuncArgs :: [(String, a)] -> Environment a -> IO ()
mkFuncArgs pairs env = modifyIORef' (getEnv env) (`Map.union` Map.fromList pairs)    -- this is called a section


assign :: String -> a -> Environment a -> EnvAction a
assign name value = \case
    Global e -> do
        env <- liftIO $ readIORef e
        case Map.lookup name env of
            Nothing -> throwError (UndefinedVar name)
            Just _  -> do
                liftIO $ modifyIORef' e (Map.insert name value)
                pure value
    Local e p -> do
        env <- liftIO $ readIORef e
        case Map.lookup name env of
            Nothing -> assign name value p
            Just _  -> do
                liftIO $ modifyIORef' e (Map.insert name value)
                pure value


emptyEnv :: Environment a -> IO (Environment a)
emptyEnv (Global e) = error "something really bad happened, delete Victim!"
emptyEnv (Local e p) = createLocalEnv p
