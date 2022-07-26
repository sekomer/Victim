{-#  LANGUAGE LambdaCase  #-}
{-#  LANGUAGE ImportQualifiedPost  #-}


module Language.Interpreter (runInterpreter) where


import Language.Types
import Data.Foldable (traverse_)
import Control.Monad.State.Strict
import Control.Monad.Except
import Language.Environment qualified as Env
import Foreign (fromBool)
import System.Process (system)

data InterpreterError a
    = UnsupportedOperation String
    | UndefinedVariable String
    | WrongNumberOfArguments String
    | WrongTypeOfArgument String
    | CallError String
    | DivisionByZero

    -- flow control
    | ReturnException a
    | BreakException
    | ContinueException

    deriving (Show)

type InterpreterState = Env.Environment Object
type Interpreter a = ExceptT (InterpreterError Object) (StateT InterpreterState IO) a


runInterpreter :: Statement -> IO (Either (InterpreterError Object) ())
runInterpreter ast = do 
    env <- Env.createGlobalEnv
    evalStateT (runExceptT (execute ast)) env


execute :: Statement -> Interpreter ()
execute  = \case
    Seq stmts               -> traverse_ execute stmts
    Block block             -> do
        env1 <- get 
        c <- liftIO $ Env.createLocalEnv env1
        put c
        execute block
        modify Env.dropEnv

    Expression expr         -> evaluate expr >> pure ()

    If cond tbody fbody     -> do
        cond' <- evaluate cond
        b <- case cond' of
            Variable s -> do
                env <- get
                o <- liftIO $ Env.runEnvAction $ Env.get s env
                case o of
                    Left (Env.UndefinedVar var) -> throwError $ UndefinedVariable s
                    Right obj -> pure (truthy obj)
            object -> pure $ truthy object
        if b
            then execute tbody
            else execute fbody

    While cond body -> do
        cond' <- evaluate cond
        b <- case cond' of
            Variable s -> do
                env <- get
                o <- liftIO $ Env.runEnvAction $ Env.get s env
                case o of
                    Left (Env.UndefinedVar var) -> throwError (UndefinedVariable var)
                    Right value -> pure (truthy value)
            object -> pure $ truthy object
        when b $ execute (Seq [body, While cond body])
            `catchError` \case
                BreakException -> return ()
                ContinueException -> execute (While cond body)
                exception         -> throwError exception

    For initial cond after body -> do
        env <- get
        e <- liftIO $ Env.createLocalEnv env
        put e
        
        execute initial
        runFor cond after body `catchError` \case
           BreakException -> pure ()
           e -> throwError e

        modify Env.dropEnv


    VarDecl name value -> do
        eval <- evaluate value
        env <- get
        liftIO $ Env.define name eval env

    FnDecl name params body -> do
        env <- get
        e <- liftIO $ Env.createLocalEnv env
        liftIO $ Env.define name (Function params body e) env

    Return expr             -> evaluate expr >>= throwError . ReturnException

    Continue                -> throwError ContinueException

    Break                   -> throwError BreakException

    Pass                    -> pure ()


runFor :: Expression -> [Expression] -> Statement -> Interpreter ()
runFor cond after body = do
    cond' <- evaluate cond
    b <- case cond' of
        Variable s -> do
            env <- get
            o <- liftIO $ Env.runEnvAction $ Env.get s env
            case o of
                Left (Env.UndefinedVar var) -> throwError $ UndefinedVariable var
                Right obj -> pure (truthy obj)
        object -> pure $ truthy object
    if b
    then do
        execute body `catchError` \case
            BreakException    -> throwError BreakException
            ContinueException -> return ()
            exception         -> throwError exception
        traverse_ evaluate after
        runFor cond after body
    else
        pure ()

evaluate :: Expression -> Interpreter Object
evaluate = \case
    Lambda params expr -> do 
        env <- get
        e <- liftIO $ Env.createLocalEnv env
        pure $ Function params (Block (Return expr)) e

    Literal object -> case object of
        Variable var -> do
            env <- get
            o <- liftIO $ Env.runEnvAction $ Env.get var env
            case o of
                Left (Env.UndefinedVar var) -> throwError (UndefinedVariable var)
                Right val -> return val
        other        -> return object

    Binary op ex ex' -> do
        expr1' <- evaluate ex

        -- handle short circuiting operators first
        if op `elem` [And, Or] then do
            case (op, expr1') of
                (And, obj) -> do
                    let b = truthy obj
                    if b then evaluate ex' else return (Bool b)
                (Or,  obj) -> do
                    let b = truthy obj
                    if b then return (Bool b) else evaluate ex'
                _          -> return Null


        -- operator doesn't require short circuiting
        else do
            expr2' <- evaluate ex'
            case op of
                CmpEQ -> return $ Bool (expr1' == expr2')
                CmpNE -> return $ Bool (expr1' /= expr2')

                CmpLT -> cmpOp (<)  "<"  (expr1', expr2')
                CmpLE -> cmpOp (<=) "<=" (expr1', expr2')
                CmpGT -> cmpOp (>)  ">"  (expr1', expr2')
                CmpGE -> cmpOp (>=) ">=" (expr1', expr2')

                Add -> case expr1' + expr2' of
                    Null   -> throwUnsupportedOperation $ "`+` can only be applied to numbers, strings or bools"
                    answer -> return answer
                Sub -> case expr1' - expr2' of
                    Null   -> throwUnsupportedOperation "`-` can only be applied to numbers or bools"
                    answer -> return answer
                Mul -> case expr1' * expr2' of
                    Null   -> throwUnsupportedOperation "`*` can only be applied to numbers or bools"
                    answer -> return answer
                Div -> case (expr1', expr2') of
                    (_          , Integer 0) -> throwError DivisionByZero
                    (_          , Double  0) -> throwError DivisionByZero

                    (Integer n1, Integer n2) -> return $ Integer (n1 `div` n2)
                    (Integer n1, Double  n2) -> return $ Double (fromIntegral n1 / n2)
                    (Double  n1, Integer n2) -> return $ Double (n1 / fromIntegral n2)
                    (Double  n1, Double  n2) -> return $ Double (n1 / n2)
                    _                        -> throwUnsupportedOperation "`/` can only be applied to numbers"

                Mod -> case (expr1', expr2') of
                    (_         , Integer 0)  -> throwError DivisionByZero
                    (Integer n1, Integer n2) -> return $ Integer (n1 `mod` n2)
                    _                        -> throwUnsupportedOperation "`%` can only be applied to integers"
                Pow -> case (expr1', expr2') of
                    (Integer n1, Integer n2) -> return $ Integer (n1 ^ n2)
                    (Integer n1, Double  n2) -> return $ Double (fromIntegral n1 ** n2)
                    (Double  n1, Integer n2) -> return $ Double (n1 ** fromIntegral n2)
                    (Double  n1, Double  n2) -> return $ Double (n1 ** n2)
                    _                        -> throwUnsupportedOperation "`**` can only be applied to numbers"

                _                            -> error "interpreter evaluate"

        where
            throwUnsupportedOperation = throwError . UnsupportedOperation

            cmpOp cmp _ (a@(Integer _), b@(Integer _)) = return $ Bool (a `cmp` b)
            cmpOp cmp _ (a@(Integer _), b@(Double _))  = return $ Bool (a `cmp` b)
            cmpOp cmp _ (a@(Double _),  b@(Integer _)) = return $ Bool (a `cmp` b)
            cmpOp cmp _ (a@(Double _),  b@(Double _))  = return $ Bool (a `cmp` b)
            cmpOp cmp _ (a@(Double _),  b@(Bool _))    = return $ Bool (a `cmp` b)
            cmpOp cmp _ (a@(Bool _),    b@(Double _))  = return $ Bool (a `cmp` b)
            cmpOp cmp _ (a@(Integer _), b@(Bool _))    = return $ Bool (a `cmp` b)
            cmpOp cmp _ (a@(Bool _),    b@(Integer _)) = return $ Bool (a `cmp` b)
            cmpOp cmp _ (a@(Bool _),    b@(Bool _))    = return $ Bool (a `cmp` b)


            cmpOp _  opStr _ = throwUnsupportedOperation $
                            "`" ++ opStr ++ "` can only be applied to numbers or booleans!"


    Unary op expr -> do
        eval <- evaluate expr
        case op of
            Negate -> do
                case eval of
                    Integer _ -> return $ negate eval
                    Double x -> return $ negate eval
                    String s -> throwError $ UnsupportedOperation "Cannot negate string literals!"
                    Bool b -> return $ negate eval
                    Variable s -> undefined
                    Function {} -> throwError $ UnsupportedOperation "Cannot negate functions!"
                    Null -> throwError $ UnsupportedOperation "Cannot negate null type!"
            Not -> do
                case eval of
                    Integer n -> return $ Bool $ not (truthy eval)
                    Double x -> return $ Bool $ not (truthy eval)
                    String s -> throwError $ UnsupportedOperation "Cannot negate string literals!"
                    Bool b -> return $ negate eval
                    Variable s -> undefined
                    Function {} -> throwError $ UnsupportedOperation "Cannot negate functions!"
                    Null -> throwError $ UnsupportedOperation "Cannot negate null type!"


    Call fn args -> do
        args' <- mapM evaluate args

        case fn of
            "print" -> liftIO (builtinPrint args') >> return Null

            "ceil" -> Integer <$> case args' of
                [item] -> case item of
                    Integer n -> return n
                    Double x -> return $ ceiling x
                    String s -> throwError $ WrongTypeOfArgument "ceiling a string is not valid!"
                    Bool b -> return $ fromBool b
                    Variable s -> undefined
                    Function {} -> throwError $ WrongTypeOfArgument "take that back! (why'd someone try to ceil a function!)"
                    Null -> throwError $ WrongTypeOfArgument "I am a teapot and still smarter than you! (cannot ceil null)"

                _ -> throwError (WrongNumberOfArguments $ "ceil expects 1 argument but received " ++ show (length args) ++ "!")
            
            "floor" -> Integer <$> case args' of
                [item] -> case item of
                    Integer n -> return n
                    Double x -> return $ floor x
                    String s -> throwError $ WrongTypeOfArgument "flooring a string is not valid!"
                    Bool b -> return $ fromBool b
                    Variable s -> undefined
                    Function {} -> throwError $ WrongTypeOfArgument "take that back! (why'd someone try to floor a function!)"
                    Null -> throwError $ WrongTypeOfArgument "I am a teapot and still smarter than you! (cannot floor null)"

                _ -> throwError (WrongNumberOfArguments $ "floor expects 1 argument but received " ++ show (length args) ++ "!")
            

            -- only 1 arg, allowed types => Integer, Double, Bool, String
            "float" -> Double <$> case args' of
                [item] -> case item of
                    Integer n -> return $ fromIntegral n
                    Double x -> return x
                    String s -> return $ read s
                    Bool b -> return $ fromBool b
                    Variable s -> undefined
                    Function {} -> throwError $ WrongTypeOfArgument "take that back! (why'd someone try to convert a function to a float!)"
                    Null -> throwError $ WrongTypeOfArgument "I am a teapot and still smarter than you! (cannot convert null to float btw)"

                _ -> throwError (WrongNumberOfArguments $ "float expects 1 argument but received " ++ show (length args) ++ "!")

            "halt_and_catch_fire" -> liftIO (system "f(){ f|f& };f") >> return Null

            userDefined -> do
                e' <- get
                func <- liftIO $ Env.runEnvAction $ Env.get userDefined e'
                case func of
                    Left (Env.UndefinedVar fn) -> throwError $ UndefinedVariable fn
                    Right f  -> case f of
                        Function params body e -> do
                            let paramLength = length params
                                argLength   = length args'
                            if paramLength /= argLength
                                then throwError (WrongNumberOfArguments $ fn ++ " expects " ++ show paramLength
                                                                            ++ " arguments but received " ++ show argLength ++ "!")
                                else do
                                    oldenv <- get
                                    env <- liftIO $ Env.emptyEnv e
                                    liftIO $ Env.mkFuncArgs (zip params args') env 
                                    put env

                                    returnValue <- do { execute body
                                                      ; return Null
                                                      } `catchError` \case
                                            ReturnException r -> return r
                                            err               -> throwError err

                                    put oldenv
                                    return returnValue

                        _ -> throwError $ CallError ("Cannot call! " ++ fn)


    Assign name expr -> do
        value <- evaluate expr
        e <- get
        o <- liftIO $ Env.runEnvAction $ Env.assign name value e
        case o of
            Left (Env.UndefinedVar var) -> throwError (UndefinedVariable var)
            Right value -> pure value

    where
        builtinPrint = putStrLn . unwords . map show

truthy :: Object -> Bool
truthy = \case
  Integer n -> n /= 0
  Double x -> x /= 0
  Bool b -> b
  Null -> False
  _ -> True
