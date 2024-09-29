{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant bracket" #-}
module LambdaParser where

import Parser
import Data.Lambda
import Data.Builder

-- | Idea taken from Stack overflow:  
-- Link: https://stackoverflow.com/questions/43594186/parse-a-string-of-length-at-least-one-character
-------
alphabet :: Parser Char
alphabet = oneof ['a' .. 'z']

-- | Taken from "Tim's code stuff - Parser Combinator"
-- Link: https://tgdwyer.github.io/parsercombinators/

chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = p >>= rest
 where
  rest a =
    (do
        f <- op
        b <- p
        rest (f a b)
      )
      ||| pure a

-- | Taken from "Tutorial Week 11 - Instances.hs"
readInt :: String -> Maybe (Int, String)
readInt s = case reads s of
  [(x, rest)] -> Just (x, rest)
  _           -> Nothing

-- | Taken from my group's code "Tutorial Week 11 - Parser.hs"
-- Link: https://github.com/arvindsiva5/fit2102-weekly-groupwork/blob/main/week11/Parser.hs
int :: Parser Int
int = P f
 where
  -- This is okay because the case statement is small
  f "" = Error UnexpectedEof
  f x  = case readInt x of
    Just (v, rest) -> Result rest v
    Nothing        -> Error $ UnexpectedChar (head x)


-- | Idea Taken from "Tim's code stuff - Parser Combinator"
-- Original Parser : Operator Parser (op)
-- Link: https://tgdwyer.github.io/parsercombinators/
spacesstr :: String -> Parser String
spacesstr c = do
   spaces
   string c
   pure c


-- You can add more imports if you need them

-- Remember that you can (and should) define your own functions, types, and
-- parser combinators. Each of the implementations for the functions below
-- should be fairly short and concise.

{-|
    Part 1
-}

-- | Exercise 1

-- | Parses a string representing a lambda calculus expression in long form


-- Part 1 (BNF Grammar for long and short lambda)
--------------------------------------------------------------------
-- <expr> ::= "(" <spaces>* <expr> <spaces>* ")" | <lambda> "." <body>
-- <lambda>  ::= <spaces>* <lambda> <spaces>* <lambda> <spaces>* | "\\" <alphabet>
-- <body> ::= <alphabet> | <expr> | "(" <body> ")" | <spaces>* <body> <spaces>* <body> <spaces>*
-- <alphabet> ::= [a-z] | <spaces>* <alphabet> <spaces>* <alphabet> <spaces>*
-- <spaces> ::= " "

-- Part 2 and 3 (Lambda calculus parser combinator)
-- Combination of long and short lambda
--------------------------------------------------------------------
-- Note : All spaces has been filtered using spacesstr function

-- Non terminal <expr>
lc :: Parser Builder
lc =  fullExpr ||| bracketLc 

-- Non terminal <lambda>
lambda :: Parser [Builder -> Builder]
lambda =  do
  a <- list alphabet        -- Take a list of lambda terms
  pure (lam <$> a)          -- Partially apply lam function to the list

-- Non terminal <alphabet>
termP :: Parser Builder
termP = do
  spaces
  term <$> alphabet 

-- Non terminal <body>
body :: Parser Builder
body = chain (termP ||| lc ||| bracketBody) (pure ap)


bracketLc :: Parser Builder
bracketLc = do
  spacesstr "("
  a <- lc
  spacesstr ")"
  pure a

bracketBody :: Parser Builder
bracketBody = do
  spacesstr "("
  a <- body
  spacesstr ")"
  pure a

fullExpr :: Parser Builder
fullExpr = do
  spacesstr "λ"
  a <- lambda
  spacesstr "."
  b <- body
  pure (foldr ($) b a) -- Reduce folds term with lambda from right to left

--
-- >>> parse longLambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse longLambdaP "(λx.(λy.xy(xx)))"
-- Result >< \xy.xy(xx)
--
-- >>> parse longLambdaP "(λx(λy.x))"
-- UnexpectedChar '('

longLambdaP :: Parser Lambda
longLambdaP = build <$> body

-- | Parses a string representing a lambda calculus expression in short form
--
-- >>> parse shortLambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse shortLambdaP "λxy.xy(xx)"
-- Result >< \xy.xy(xx)
--
-- >>> parse shortLambdaP "λx.x(λy.yy)"
-- Result >< \x.x\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy
--
-- >>> parse shortLambdaP "λxyz"
-- UnexpectedEof

shortLambdaP :: Parser Lambda
shortLambdaP = build <$> body

-- | Parses a string representing a lambda calculus expression in short or long form
-- >>> parse lambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse lambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse lambdaP "λx..x"
-- UnexpectedChar '.'
--

lambdaP :: Parser Lambda
lambdaP = build <$> body

{-|
    Part 2
-}

-- | Exercise 1

-- Builder lambda expression for logical statements
------
trueE :: Builder
trueE = boolToLam True

falseE :: Builder
falseE = boolToLam False

ifE :: Builder
ifE = lam 'b' $ lam 't' $ lam 'f' (term 'b' `ap` term 't' `ap` term 'f')

andE :: Builder
andE = lam 'x' $ lam 'y' (ifE `ap` term 'x' `ap` term 'y' `ap` falseE)

orE :: Builder
orE = lam 'x' $ lam 'y' (ifE `ap` term 'x' `ap` trueE `ap` term 'y')

notE :: Builder
notE = lam 'x' (ifE `ap` term 'x' `ap` falseE `ap` trueE)
-------

-- Parsers for logical statements
------
boolP :: Parser Builder
boolP = (spacesstr "True" >> pure trueE) ||| (spacesstr "False" >> pure falseE)

orP :: Parser (Builder -> Builder -> Builder)
orP = spacesstr "or" >> pure (ap . (orE `ap`))

andP :: Parser (Builder -> Builder -> Builder)
andP =  spacesstr "and" >> pure (ap . (andE `ap`))

notP :: Parser (Builder -> Builder)
notP = spacesstr "not" >> pure (notE `ap`)

ifP :: Parser (Builder -> Builder -> Builder -> Builder)
ifP = spacesstr "if" >> pure ((ap .) . ap . (ifE `ap`))             
-------

-- Parsers combinators for logical statements
------
logicPC :: Parser Builder
logicPC = chain (notPC ||| boolP ||| ifPC ||| complexCalcPC) (andP ||| orP)

notPC :: Parser Builder
notPC = do 
  a <- notP
  a <$> logicPC

ifPC :: Parser Builder
ifPC = do
  spaces
  a <- ifP
  b <- logicPC
  spacesstr "then"
  c <- logicPC
  spacesstr "else"
  a b c <$> logicPC
  
 -------

-- IMPORTANT: The church encoding for boolean constructs can be found here -> https://tgdwyer.github.io/lambdacalculus/#church-encodings

-- | Parse a logical expression and returns in lambda calculus
-- >>> lamToBool <$> parse logicP "True and False"
-- Result >< Just False
--
-- >>> lamToBool <$> parse logicP "True and False or not False and True"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "not not not False"
-- Result >< Just True
--
-- >>> parse logicP "True and False"
-- Result >< (\xy.(\btf.btf)xy\_f.f)(\t_.t)\_f.f
--
-- >>> parse logicP "not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f
-- >>> lamToBool <$> parse logicP "if True and not False then True or True else False"
-- Result >< Just True

logicP :: Parser Lambda
logicP = build <$> logicPC


-- | Exercise 2

-- | The church encoding for arithmetic operations are given below (with x and y being church numerals)

-- | x + y = add = λxy.y succ x
-- | x - y = minus = λxy.y pred x
-- | x * y = multiply = λxyf.x(yf)
-- | x ** y = exp = λxy.yx

-- | The helper functions you'll need are:
-- | succ = λnfx.f(nfx)
-- | pred = λnfx.n(λgh.h(gf))(λu.x)(λu.u)
-- | Note since we haven't encoded negative numbers pred 0 == 0, and m - n (where n > m) = 0

-- Helper functions coded with lambda expressions
------
succE :: Builder
succE = lam 'n' $ lam 'f' $ lam 'x' (term 'f' `ap` (term 'n' `ap` term 'f' `ap` term 'x'))

predE :: Builder
predE = lam 'n' $ lam 'f' $ lam 'x' (term 'n' `ap` (lam 'g' $ lam 'h' (term 'h' `ap` (term 'g' `ap` term 'f'))) `ap` (lam 'u' $ term 'x') `ap` lam 'u' (term 'u')) 
-------

-- Lambda expressions for basic arithmetics
------
plusE :: Builder
plusE = lam 'x' $ lam 'y' (term 'y' `ap` succE `ap` term 'x')

minusE :: Builder
minusE = lam 'x' $ lam 'y' (term 'y' `ap` predE `ap` term 'x')
-------

-- Parsers for numbers and basic arithmetics
------
numberP :: Parser Builder
numberP = do
    spaces
    intToLam <$> int

-- Note: Plus and minus symbol has the same precedence
addminusP :: Parser (Builder -> Builder -> Builder)
addminusP = (spacesstr "+" >> pure (ap . (plusE `ap`))) ||| (spacesstr "-" >> pure (ap . (minusE `ap`)))
-------

-- | Parse simple arithmetic expressions involving + - and natural numbers into lambda calculus
-- >>> lamToInt <$> parse basicArithmeticP "5 + 4"
-- Result >< Just 9
--
-- >>> lamToInt <$> parse basicArithmeticP "5 + 9 - 3 + 2"
-- Result >< Just 13

basicArithmeticP :: Parser Lambda
basicArithmeticP = build <$> chain numberP addminusP    -- Parser combinator for basic arithmetics


-- Lambda expressions for complex arithmetics
------
timesE :: Builder
timesE = lam 'x' $ lam 'y' $ lam 'f' (term 'x' `ap` (term 'y' `ap` term 'f'))

powerE :: Builder
powerE = lam 'x' $ lam 'y' (term 'y' `ap` term 'x')
------

-- Parsers for brackets and complex arithmetics
------
timesP :: Parser (Builder -> Builder -> Builder)
timesP = spacesstr "*" >> pure (ap . (timesE `ap`))

powerP :: Parser (Builder -> Builder -> Builder)
powerP = spacesstr "**" >> pure (ap . (powerE `ap`))

bracketP :: Parser Builder
bracketP = do 
  spacesstr "("
  a <- logicPC
  spacesstr ")"
  pure a
-------

-- Parser combinators for complex arithmetics (Combination of basic and complex)
------
arithmeticPC :: Parser Builder
arithmeticPC = chain timesPC addminusP

timesPC :: Parser Builder
timesPC = chain powerPC timesP

powerPC :: Parser Builder
powerPC = chain (bracketP ||| numberP) powerP
-------

-- | Parse arithmetic expressions involving + - * ** () and natural numbers into lambda calculus
-- >>> lamToInt <$> parse arithmeticP "5 + 9 * 3 - 2**3"
-- Result >< Just 24
--
-- >>> lamToInt <$> parse arithmeticP "100 - 4 * 2**(4-1)"
-- Result >< Just 68

arithmeticP :: Parser Lambda
arithmeticP = build <$> arithmeticPC


-- | Exercise 3

-- | The church encoding for comparison operations are given below (with x and y being church numerals)

-- | x <= y = LEQ = λmn.isZero (minus m n)
-- | x == y = EQ = λmn.and (LEQ m n) (LEQ n m)

-- | The helper function you'll need is:
-- | isZero = λn.n(λx.False)True


-- Helper functions coded with lambda expressions
------
isZero :: Builder
isZero = lam 'n' (term 'n' `ap` (lam 'x' falseE) `ap` trueE)
-------

-- Lambda expressions for complex conditionals
------
leqE :: Builder
leqE = lam 'm' $ lam 'n' (isZero `ap` (minusE `ap` term 'm' `ap` term 'n'))

geqE :: Builder 
geqE = lam 'm' $ lam 'n' (leqE `ap` term 'n' `ap` term 'm')

greaterE :: Builder
greaterE = lam 'm' $ lam 'n' (notE `ap` (isZero `ap` (minusE `ap` term 'm' `ap` term 'n')))

lesserE :: Builder
lesserE = lam 'm' $ lam 'n' (greaterE `ap` term 'n' `ap` term 'm')

eqE :: Builder
eqE = lam 'm' $ lam 'n' (andE `ap` (leqE `ap` term 'm' `ap` term 'n') `ap` (leqE `ap` term 'n' `ap` term 'm'))

noteqE :: Builder
noteqE = lam 'm' $ lam 'n' (notE `ap` eqE)

-- Parsers for complex comparisons
------

-- Note : Comparison operators such as <=, >=, >, < has the same precedences
compareP :: Parser (Builder -> Builder -> Builder)
compareP = (spacesstr "<=" >> pure (ap . (leqE `ap`))) ||| (spacesstr ">=" >> pure (ap . (geqE `ap`))) ||| (spacesstr ">" >> pure (ap . (greaterE `ap`))) ||| (spacesstr "<" >> pure (ap . (lesserE `ap`)))

-- Note : Equality operators such as ==, != has the same precedences
equalityP :: Parser (Builder -> Builder -> Builder)
equalityP = (spacesstr "==" >> pure (ap . (eqE `ap`))) ||| (spacesstr "!=" >> pure (ap . (noteqE `ap`)))
--------

-- Parser combinator for complex comparisons (Combination of logical statements, arithmetics and comparisons)
------
-- (cont. from logicP)

complexCalcPC :: Parser Builder
complexCalcPC = chain comparePC equalityP

comparePC :: Parser Builder
comparePC = chain arithmeticPC compareP


-- >>> lamToBool <$> parse complexCalcP "9 - 2 <= 3 + 6"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "15 - 2 * 2 != 2**3 + 3 or 5 * 3 + 1 < 9"
-- Result >< Just False
complexCalcP :: Parser Lambda
complexCalcP = build <$> logicPC


{-|
    Part 3
-}

-- | Exercise 1

-- | The church encoding for list constructs are given below
-- | [] = null = λcn.n
-- | isNull = λl.l(λht.False) True
-- | cons = λhtcn.ch(tcn)
-- | head = λl.l(λht.h) False
-- | tail = λlcn.l(λhtg.gh(tc))(λt.n)(λht.t)

-- Lambda expressions for list operations
------
nullE :: Builder
nullE = lam 'c' $ lam 'n' (term 'n')

isnullE :: Builder 
isnullE = lam 'l' (term 'l' `ap` (lam 'h' $ lam 't' falseE) `ap` trueE)

consE :: Builder
consE = lam 'h' $ lam 't' $ lam 'c' $ lam 'n' (term 'c' `ap` term 'h' `ap` (term 't' `ap` term 'c' `ap` term 'n'))

headE :: Builder
headE = lam 'l' (term 'l' `ap` (lam 'h' $ lam 't' (term 'h')) `ap` falseE)

tailE :: Builder
tailE = lam 'l' $ lam 'c' $ lam 'n' (term 'l' `ap` (lam 'h' $ lam 't' $ lam 'g' (term 'g' `ap` term 'h' `ap`(term 't' `ap` term 'c'))) `ap` (lam 't' (term 'n')) `ap` (lam 'h' $ lam 't' (term 't')))
--------

-- Parsers and parser combinator for lists
-------

-- Parser combinator
listPC :: Parser Builder 
listPC = chain (emptylist ||| oneElem ||| multiElem) (pure ap)


-- Empty list parser
emptylist :: Parser Builder
emptylist = do
  spacesstr "["
  spaces
  spacesstr "]"
  pure nullE

-- One element parser
oneElem :: Parser Builder
oneElem = do
  spacesstr "["
  a <- listOpPC     -- Refer to the implementation below
  spacesstr "]"
  pure (consE `ap` a `ap` nullE)

-- Multi elements parser
multiElem :: Parser Builder
multiElem = do
  spacesstr "["
  a <- listOpPC     -- Refer to the implementation below
  b <- restElem
  pure (consE `ap` a `ap` b)

-- Rest of the element (cont. multiElem)
restElem :: Parser Builder
restElem = chain (lastElemPC ||| restElemPC) (pure ap)

restElemPC :: Parser Builder
restElemPC = do
  spacesstr ","
  a <- listOpPC

  
  pure (consE `ap` a)

lastElemPC :: Parser Builder
lastElemPC = do
  spacesstr ","
  a <- listOpPC
  spacesstr "]"
  pure (consE `ap` a `ap` nullE)

-------

--
-- >>> parse listP "[]"
-- Result >< \cn.n
--
-- >>> parse listP "[True]"
-- Result >< (\htcn.ch(tcn))(\xy.x)\cn.n
--
-- >>> parse listP "[0, 0]"
-- Result >< (\htcn.ch(tcn))(\fx.x)((\htcn.ch(tcn))(\fx.x)\cn.n)
--
--
-- >>> parse listP "[0, 0"
-- UnexpectedEof

listP :: Parser Lambda
listP = build <$> listPC

-- Parsers for simple list operations
------
isNullP :: Parser (Builder -> Builder)
isNullP = spacesstr "isNull" >> pure (isnullE `ap`)

headandtailP :: Parser Builder
headandtailP = (spacesstr "head" >> pure headE) ||| ((spacesstr "tail" ||| spacesstr "rest") >> pure tailE)
-------

--- Parser combinator for list operations
------
listOpPC :: Parser Builder
listOpPC = chain (inListPC ||| isNullExpr ||| headandtailExpr) (pure ap)

inListPC :: Parser Builder
inListPC = chain (listPC ||| logicPC) (pure ap)

headandtailExpr :: Parser Builder
headandtailExpr = do
  a <- headandtailP
  b <- listOpPC
  pure(a `ap` (b)) 

isNullExpr :: Parser Builder
isNullExpr = do
  a <- isNullP  
  a <$> listOpPC
-------

-- >>> lamToBool <$> parse listOpP "head [True, False, True, False, False]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head rest [True, False, True, False, False]"
-- Result >< Just False
--
-- >>> lamToBool <$> parse listOpP "isNull []"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "isNull [1, 2, 3]"
-- Result >< Just False
listOpP :: Parser Lambda
listOpP = build <$> listOpPC


-- | Exercise 2

-- | Implement your function(s) of choice below!
