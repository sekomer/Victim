{-# LANGUAGE  ImportQualifiedPost  #-}

module Language.Parser (parseCode) where

import Language.Types

import Text.Parsec
import Text.Parsec.Expr
    ( buildExpressionParser
    , Assoc(AssocRight, AssocLeft)
    , Operator(Infix, Prefix) )

import Text.Parsec.Token qualified as Token
import Text.Parsec.Language ( emptyDef )
import GHC.ByteOrder (ByteOrder(LittleEndian))


type Parser = Parsec String ()


parseCode :: String -> IO Statement
parseCode code = do
    case parse (padSpaces parseProgram <* eof) "" code of
      Left  err -> print err >> fail "crap"
      Right res -> return res



parseProgram :: Parser Statement
parseProgram = parseStatements

parseStatements :: Parser Statement
parseStatements = Seq <$> many parseStatement

parseStatement :: Parser Statement
parseStatement = padSpaces $ choice
   [ parseBlock
   , parseFor
   , parseVarDecl
   , parseIfStmt
   , parseExpression
   , parseWhile
   , parseFunc
   , parseReturn
   , parseBreak
   , parseContinue
   , parseSwitch
   , parseEmpty ]


{-
    for (; i < cond; i++)

-}
parseFor :: Parser Statement
parseFor = do
    reserved "for"
    (initial, cond, after) <- parens $ do
        initial <- parseVarDecl <|> parseEmpty
        cond <- expression <|> pure (Literal (Bool True))
        padSpaces semi
        after <- sepBy expression (padChar ',')
        return (initial, cond, after)
    For initial cond after `fmap` parseStatement


parseEmpty :: Parser Statement
parseEmpty = do
    padSpaces semi
    return Pass

parseSwitch :: Parser Statement
parseSwitch = do
    reserved "case"
    e' <- parens $ padSpaces expression

    padChar '{'
    cases <- many1 $ do
        reserved "when"
        cond <- padSpaces expression
        reservedOp "=>"
        body <- parseStatement
        pure (cond, body)

    last' <- (reserved "otherwise" >> reservedOp "=>" >> parseStatement) <|> pure Pass

    padChar '}'

    return $ Seq [ VarDecl tmpVarName e', convert2If cases last']
    where
        convert2If :: [(Expression, Statement)] -> Statement -> Statement
        convert2If [] l = l
        convert2If ((e,s):es) l = If (Binary CmpEQ tmpVar e) s (convert2If es l)

        tmpVar = Literal (Variable tmpVarName)
        tmpVarName = "%match"


parseBreak :: Parser Statement
parseBreak = do
    reserved "break"
    padSpaces semi
    return Break

parseContinue :: Parser Statement
parseContinue = do
    reserved "continue"
    padSpaces semi
    return Continue

parseFunc :: Parser Statement
parseFunc = do
    reserved "fn"
    ident <- identifier
    params <- parens (sepBy identifier (padChar ','))
    body <- parseBlock
    return $ FnDecl ident params body


parseReturn :: Parser Statement
parseReturn = do
    reserved "return"
    expr <- expression <|> pure (Literal Null)
    padSpaces semi
    return $ Return expr


parseWhile :: Parser Statement
parseWhile = do
    reserved "while"
    cond <- parens expression
    body <- parseStatement
    return $ While cond body

parseVarDecl :: Parser Statement
parseVarDecl = do
    reserved "var"
    ident <- identifier
    reservedOp ":="
    expr <- expression
    padSpaces semi
    return $ VarDecl ident expr


parseIfStmt :: Parser Statement
parseIfStmt = do
    reserved "if"
    cond <- padSpaces (parens expression)
    tbody <- parseStatement
    fbody <- (reserved "else" *> parseStatement <|> pure Pass)
    return $ If cond tbody fbody


parseBlock :: Parser Statement
parseBlock = Block <$> braces parseStatements


parseExpression :: Parser Statement
parseExpression = Expression <$> expression <* (padSpaces semi)


expression :: Parser Expression
expression = binaryExpression


binaryExpression :: Parser Expression
binaryExpression = buildExpressionParser binaryOperators binaryTerm

binaryOperators =
    [ [Prefix (reservedOp "-"  >> return (Unary Negate ))           ,
       Prefix (reservedOp "!"  >> return (Unary Not    ))           ]

    , [Infix  (reservedOp "**" >> return (Binary Pow   )) AssocRight]

    , [Infix  (reservedOp "*"  >> return (Binary Mul   )) AssocLeft ,
       Infix  (reservedOp "/"  >> return (Binary Div   )) AssocLeft ,
       Infix  (reservedOp "%"  >> return (Binary Mod   )) AssocLeft ]

    , [Infix  (reservedOp "+"  >> return (Binary Add   )) AssocLeft ,
       Infix  (reservedOp "-"  >> return (Binary Sub   )) AssocLeft ]

    , [Infix  (reservedOp ">"  >> return (Binary CmpGT )) AssocLeft ,
       Infix  (reservedOp ">=" >> return (Binary CmpGE )) AssocLeft ,
       Infix  (reservedOp "<"  >> return (Binary CmpLT )) AssocLeft ,
       Infix  (reservedOp "<=" >> return (Binary CmpLE )) AssocLeft ]

    , [Infix  (reservedOp "==" >> return (Binary CmpEQ )) AssocLeft ,
       Infix  (reservedOp "!=" >> return (Binary CmpNE )) AssocLeft ]

    , [Infix  (reservedOp "&&" >> return (Binary And   )) AssocRight]
    , [Infix  (reservedOp "||" >> return (Binary Or    )) AssocRight]
    ]

binaryTerm =  parens binaryExpression
          <|> parseLiteral
          <|> parseAnon
          <|> try parseCall
          <|> try parseAssign
          <|> Literal . Variable <$> identifier


parseLiteral :: Parser Expression
parseLiteral = Literal <$> padSpaces parseObject

parseAnon :: Parser Expression
parseAnon = do
   reserved "anon"
   params <- sepBy identifier (padChar ',')
   reservedOp "->"
   expr <- expression
   return $ Lambda params expr


parseCall :: Parser Expression
parseCall = do
   ident  <- identifier
   args <- parens (sepBy expression (padChar ','))
   return $ Call ident args

parseAssign :: Parser Expression
parseAssign = do
   ident <- identifier
   reservedOp ":="
   expr <- expression
   return $ Assign ident expr


parseObject :: Parser Object
parseObject = choice
   [ parseNumber
   , parseBool
   , parseString
   , parseNull ]
   where
      parseNumber :: Parser Object
      parseNumber = do
            number <- naturalOrFloat
            pure $ case number of                 -- either Integer Double <$> naturalOrFloat
               Left  int -> Integer int
               Right flt -> Double  flt

      parseBool :: Parser Object
      parseBool = Bool <$> choice
         [ reserved "true"  >> return True
         , reserved "false" >> return False ]

      parseString :: Parser Object
      parseString = String <$> stringLiteral

      parseNull :: Parser Object
      parseNull = Null <$ reserved "null"


padSpaces x = whiteSpace *> x <* whiteSpace
padChar = padSpaces . char


identifier     = Token.identifier       lexer       -- parses an identifier
reserved       = Token.reserved         lexer       -- parses a reserved name
reservedOp     = Token.reservedOp       lexer       -- parses an operator
parens         = Token.parens           lexer       -- parses surrounding parenthesis
braces         = Token.braces           lexer       -- parses surrounding braces
brackets       = Token.brackets         lexer       -- pasese surrounding brackets
semi           = Token.semi             lexer       -- parses a semicolon
whiteSpace     = Token.whiteSpace       lexer       -- parses whitespace
naturalOrFloat = Token.naturalOrFloat   lexer       -- parses a natural number or float
stringLiteral  = Token.stringLiteral    lexer       -- parses a string literal



lexer = Token.makeTokenParser $
    emptyDef { Token.commentStart    = "{-"
             , Token.commentEnd      = "-}"
             , Token.commentLine     = "--"
             , Token.nestedComments  = False
             , Token.identStart      = letter <|> char '_'
             , Token.identLetter     = alphaNum <|> char '_'
             , Token.reservedNames   = [ "true", "false"
                                       , "if", "else", "case", "otherwise", "when"
                                       , "while"
                                       , "var", "null"
                                       , "anon"
                                       , "fn", "return"
                                       , "break", "continue" ]
             , Token.reservedOpNames = [ "-", "!", ":="
                                       , "**"
                                       , "->", "=>"
                                       , "*", "/", "+", "-", "%"
                                       , ">", ">=", "<", "<="
                                       , "==", "!="
                                       , "&&" , "||" ]
             , Token.caseSensitive   = True }

