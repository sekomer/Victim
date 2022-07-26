
{-# LANGUAGE  LambdaCase  #-}


module Language.Types where

import Language.Environment (Environment)

{-
    Statement Data Type
-}
data Statement 
    = Seq [Statement]
    | Block Statement
    | Expression Expression
    | If Expression Statement Statement
    | While Expression Statement
    | For Statement Expression [Expression] Statement
    | VarDecl String Expression
    | FnDecl String [String] Statement
    | Return Expression
    | Continue
    | Break
    | Pass
    deriving (Show)

{-
    Expression Data Type
-}
data Expression 
    = Literal Object
    | Binary BinOp Expression Expression
    | Unary UnOp Expression
    | Call String [Expression]
    | Assign String Expression
    | Lambda [String] Expression
    deriving (Eq, Show)


data UnOp
    = Negate
    | Not
    deriving (Eq, Show)


{-

-}
data BinOp 
    = CmpEQ | CmpNE | CmpLT | CmpLE | CmpGT | CmpGE
    | And | Or
    | Add | Sub | Mul | Div | Mod | Pow
    deriving (Show, Eq)



{-
    Haskell is lazily evaluated therefore,
    the exclamation mark is needed to make the field strict. 
-}
data Object
    = Integer  !Integer
    | Double   !Double
    | String   !String
    | Bool     !Bool
    | Variable !String
    | Function ![String] Statement (Environment Object)  -- parameters, body, environment
    | Null


-- implement Show class for Object
instance Show Object where
    show = \case
        Integer i      -> show i
        Double d       -> show d
        Bool b         -> show b
        Variable v     -> show v
        Function p b e -> "<fn>"
        Null           -> "null"
        String s       -> s



instance Eq Object where
    (==) (Integer i1) (Integer i2) = i1 == i2
    (==) (Integer i1) (Double d2)  = fromIntegral i1 == d2
    (==) (Double d1)  (Integer i2) = d1 == fromIntegral i2
    (==) (Double d1)  (Double d2)  = d1 == d2
    (==) (String s1)  (String s2)  = s1 == s2
    (==) (Bool b1)    (Bool b2)    = b1 == b2
    (==) (Bool True)  (Integer 1)  = True
    (==) (Integer 1)  (Bool True)  = True   
    (==) (Bool False) (Integer 0)  = True
    (==) (Integer 0)  (Bool False) = True
    (==) (Bool True)  (Double 1)   = True
    (==) (Double 1)   (Bool True)  = True   
    (==) (Bool False) (Double 0)   = True
    (==) (Double 0)   (Bool False) = True
    (==) Null         Null         = True
    (==) _            _            = False


instance Ord Object where
    (<=) (Integer i1) (Integer i2) = i1 <= i2
    (<=) (Integer i1) (Double d2)  = fromIntegral i1 <= d2
    (<=) (Double d1)  (Integer i2) = d1 <= fromIntegral i2
    (<=) (Double d)   (Bool b)     = d <= bvalue b
    (<=) (Bool b)     (Double d)   = bvalue b <= d
    (<=) (Bool b)     (Integer i)  = bvalue b <= i
    (<=) (Integer i)  (Bool b)     = i <= bvalue b    
    (<=) (Bool d1)    (Bool d2)    = bvalue d1 <= bvalue d2
    (<=) _            _            = undefined

bvalue :: Num a => Bool -> a
bvalue False = 0
bvalue True  = 1

instance Num Object where
    (+) (Integer i1) (Integer i2) = Integer (i1 + i2)
    (+) (Integer i1) (Double d2)  = Double (fromIntegral i1 + d2)
    (+) (Double d1)  (Integer i2) = Double (d1 + fromIntegral i2)
    (+) (Double d1)  (Double d2)  = Double (d1 + d2)
    (+) (String s1)  (String s2)  = String (s1 ++ s2)
    (+) (Integer i1) (Bool i2)    = Integer (i1 + bvalue i2)
    (+) (Bool i1)    (Integer i2) = Integer (bvalue i1 + i2)
    (+) (Bool i1)    (Bool i2)    = Integer (bvalue i1 + bvalue i2)
    (+) _            _            = Null

    (-) (Integer i1) (Integer i2) = Integer (i1 - i2)
    (-) (Integer i1) (Double d2)  = Double (fromIntegral i1 - d2)
    (-) (Double d1)  (Integer i2) = Double (d1 - fromIntegral i2)
    (-) (Double d1)  (Double d2)  = Double (d1 - d2)
    (-) (Integer i1) (Bool i2)    = Integer (i1 - bvalue i2)
    (-) (Bool i1)    (Integer i2) = Integer (bvalue i1 - i2)
    (-) (Bool i1)    (Bool i2)    = Integer (bvalue i1 - bvalue i2)
    (-) _            _            = Null

    (*) (Integer i1) (Integer i2) = Integer (i1 * i2)
    (*) (Integer i1) (Double d2)  = Double (fromIntegral i1 * d2)
    (*) (Double d1)  (Integer i2) = Double (d1 * fromIntegral i2)
    (*) (Double d1)  (Double d2)  = Double (d1 * d2)
    (*) (Integer i1) (Bool i2)    = Integer (i1 * bvalue i2)
    (*) (Bool i1)    (Integer i2) = Integer (bvalue i1* i2)
    (*) (Bool i1)    (Bool i2)    = Integer (bvalue i1* bvalue i2)
    (*) _            _            = Null

    negate (Integer i) = Integer (negate i)
    negate (Double d)  = Double (negate d)
    negate (Bool b)    = Bool (not b)
    negate _           = Null

    abs (Integer i) = Integer (abs i)
    abs (Double d)  = Double (abs d)
    abs (Bool b)    = Integer (bvalue b)
    abs _           = Null

    signum (Integer i) = Integer (signum i)
    signum (Double d)  = Double (signum d)
    signum (Bool b)    = Integer 1
    signum _           = Null

    fromInteger = Integer

