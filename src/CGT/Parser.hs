module CGT.Parser
  ( valueParser
  , eitherValueParser
  , expression
  , single
  , rationalP
  , parseInt
  , listParser
  ) where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Typeable
import Numeric
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator

import qualified Data.HashSet as HS

import CGT.Values

expression = do
  res <- signedSep single (oneOf "+-") '+'
  return $ negateAndSum res
  where
    negateAndSum :: [(Char, GameValue)] -> GameValue
    negateAndSum = foldl' nasStep (number 0)
    nasStep :: GameValue -> (Char, GameValue) -> GameValue
    nasStep g1 ('+', g2) = g1 `dsum` g2
    nasStep g1 ('-', g2) = g1 `dsum` (neg g2)

single = spaces >>
         (try offNimP <|>
         try nimberP <|>
         try (liftM number rationalP) <|> try (liftM number integerP) <|>
         setP <?> "Could not find game value")

offNimP = do
  num <- try rationalP <|> integerP
  (char '*')
  nim <- (optionMaybe parseInt)
  return $ offNim num $ fromMaybe 1 nim

nimberP = do
  (char '*')
  nim <- (optionMaybe parseInt)
  return $ nimber $ fromMaybe 1 nim

rationalP = do
  num <- parseInt
  (char '/')
  den <- parseInt
  return $ dyRat $ num % den

integerP = liftM dyInt parseInt

parseInt = do
  sign <- (optionMaybe (char '-'))
  res  <- many1 digit
  return $ (signWrap sign) * (readWrap res)
  where
    signWrap :: Maybe Char -> Integer
    signWrap (Just _) = -1
    signWrap (Nothing) = 1
    readWrap :: String -> Integer
    readWrap s
      |[(n, "")] <- readDec s = n
      |otherwise = error "How did this happen?"

setP = do
  (char '{')
  setList <- many1 (try singleSetP)
  lastSet <- endSetP
  return $ gameFin setList lastSet

singleSetP = do
  spaces
  newLeft <- expression `sepBy` (char ',')
  newSep <- many1 (char '|')
  return (newLeft, length newSep)

endSetP = do
  spaces
  lastLeft <- expression `sepBy` (char ',')
  char '}'
  return lastLeft

gameFin :: [([GameValue], Int)] -> [GameValue] -> GameValue
gameFin tupList rightList =  finisher (gameInc [] tupList) rightList
  where
    gameInc :: [([GameValue], Int)] -> [([GameValue], Int)] -> [([GameValue], Int)]
    gameInc workList [] = workList
    gameInc []       (r:restList) = gameInc [r] restList
    gameInc w@((lastLeft, lastSep):workList) (n@(newLeft, newSep):restList)
      |newSep < lastSep = gameInc (n:w) restList
      |otherwise        = gameInc workList (([game (HS.fromList lastLeft) (HS.fromList newLeft)], newSep):restList)
    finisher :: [([GameValue], Int)] -> [GameValue] -> GameValue
    finisher [] [x] = x
    finisher ((lastLeft, _):finishList) rightMost = finisher finishList [game (HS.fromList lastLeft) (HS.fromList rightMost)]

baseDualList = do
  left <- expression `sepBy` (char ',')
  (char '|')
  right <- expression `sepBy` (char ',')
  return $ game (HS.fromList left) (HS.fromList right)

signedSep item seps sep = do
  first <- item
  next <- restSignedSep item seps
  return ((sep, first) : next)

restSignedSep item seps = do
    (seps >>= signedSep item seps)
    <|> (return [])

eitherValueParser :: String -> Either ParseError GameValue
eitherValueParser = parse expression "(unknown)"

valueParser :: String -> GameValue
valueParser input
  |Left e <- result = error (show e)
  |Right g <- result = g
  where
    result = eitherValueParser input

listParser :: String -> [GameValue]
listParser input
  |Left e <- result = error (show e)
  |Right g <- result = g
  where
    listExpression = between (char '[') (char ']') $ expression `sepBy` (char ',')
    eitherListParser = parse listExpression "(unknown)"
    result = eitherListParser input
