{-#  LANGUAGE LambdaCase  #-}
{-# LANGUAGE ImportQualifiedPost #-}


module Language.Environment 
( Environment                   -- Environment Type
, createGlobalEnv               -- Environment
, createEnv                     -- Environment
, dropEnv                       -- Environment -> Environment
, get                           -- String -> Environment -> Maybe Object
, define                        -- String -> Object -> Environment -> Environment
, assign                        -- String -> Object -> Environment -> (Environment, Bool)
, mkFuncArgs                    -- [(String, Object)] -> Environment -> Environment
) where

import Language.Types ( Object )
import Data.Map.Strict         (Map)
import Data.Map.Strict qualified as Map


type Env = Map String Object

data Environment
    = Global Env
    | Local  Env Environment    -- local, parent

createGlobalEnv :: Environment
createGlobalEnv = Global Map.empty

createEnv :: Environment -> Environment
createEnv = Local Map.empty

dropEnv :: Environment -> Environment
dropEnv = \case
    Global _ -> error "cannot drop global env!"
    Local _ p -> p

get :: String -> Environment -> Maybe Object
get name = \case 
    Global e   -> Map.lookup name e
    Local  e p -> 
        case Map.lookup name e of 
            Nothing    -> get name p
            Just value -> Just value


define :: String -> Object -> Environment -> Environment
define name value = \case
    Global e   -> Global (Map.insert name value e)
    Local  e p -> Local  (Map.insert name value e) p


mkFuncArgs :: [(String, Object)] -> Environment -> Environment
mkFuncArgs pairs = Local (Map.fromList pairs)


assign :: String -> Object -> Environment -> (Environment, Bool)
assign name value = \case
    Global e -> case Map.lookup name e of
        Nothing -> (Global e, False)
        Just _  -> (Global (Map.insert name value e), True)
    Local e p1 -> case Map.lookup name e of
        Nothing -> 
            let (p, b) = assign name value p1
             in (Local e p, b)
        Just _  -> (Local (Map.insert name value e) p1, True)
