{-# LANGUAGE BangPatterns #-}
module CGT.Values
    ( Dyadic (..)
    , dyInt
    , dyRat
    , GameValue (..)
    , number
    , nimber
    , offNim
    , game
    , toSidedForm
    , leftOptions
    , rightOptions
    , greater_eq
    , less_eq
    , equals
    , greater
    , less
    , confused
    , canonicalForm
    , dsum
    , neg
    , dsub
    , starCool
    , starProj
    , reducedCanonicalForm
    , lstop
    , rstop
    , infGeq
    ) where

import Data.Ratio
import Data.List
import Data.Bits
import Control.Monad
import Data.Maybe

import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

-- Making a general dyadic number type
data Dyadic = DyInt !Integer | DyRat !Rational
  deriving (Show, Eq)

-- We use a bitwise and to check if an integer is a power of 2
isPowerOfTwo :: Integer -> Bool
isPowerOfTwo n = n .&. (n-1) == 0

-- Making a smart constructor for DyInt. Not really necessary, but it's more consistent since dyRat uses a smart constructor
dyInt :: Integer -> Dyadic
dyInt = DyInt

-- Making a smart constructor for DyRat, for simplification and correctness
dyRat :: Rational -> Dyadic
dyRat r
  |q == 1 = dyInt p
  |isPowerOfTwo q = DyRat r
  |otherwise = error "Non-Dyadic Rational passed to dyRat"
  where
    p = numerator r
    q = denominator r

instance Num Dyadic where
  (DyInt a) + (DyInt b) = DyInt $ a + b
  (DyInt a) + (DyRat b) = dyRat $ (fromInteger a) + b
  (DyRat a) + (DyInt b) = dyRat $ a + (fromInteger b)
  (DyRat a) + (DyRat b) = dyRat $ a + b

  (DyInt a) * (DyInt b) = DyInt $ a * b
  (DyInt a) * (DyRat b) = dyRat $ (fromInteger a) * b
  (DyRat a) * (DyInt b) = dyRat $ a * (fromInteger b)
  (DyRat a) * (DyRat b) = dyRat $ a * b

  abs (DyInt a) = DyInt $ abs a
  abs (DyRat a) = DyRat $ abs a

  signum (DyInt a) = DyInt $ signum a
  signum (DyRat a) = dyRat $ signum a

  fromInteger = DyInt

  negate (DyInt a) = DyInt $ negate a
  negate (DyRat a) = DyRat $ negate a

instance Ord Dyadic where
  (DyInt a) <= (DyInt b) = a <= b
  (DyRat a) <= (DyInt b) = a <= fromInteger b
  (DyInt a) <= (DyRat b) = fromInteger a <= b
  (DyRat a) <= (DyRat b) = a <= b

instance Fractional Dyadic where
  (DyInt a) / (DyInt b)  = dyRat $ a % b
  (DyInt a) / (DyRat b)  = dyRat $ fromInteger a / b
  (DyRat a) / (DyInt b)  = dyRat $ a / fromInteger b
  (DyRat a) / (DyRat b)  = dyRat $ a / b
  fromRational = dyRat

-- Defining the datatype GameValue
data GameValue = Number  !Dyadic |
                 Nimber  !Integer |
                 OffNim  !Dyadic !Integer |
                 -- There is a Set datatype available in Haskell, but it expects its elements to be able to be ordered, so we will use lists instead
                 Game {left :: HS.HashSet GameValue, right :: HS.HashSet GameValue}

instance Show GameValue where
  show (Number (DyInt x))   = show x
  show (Number (DyRat x))   = (show . numerator) x ++ "/" ++ (show . denominator) x
  show (Nimber m)           = '*' : maybeShow m
    where
      maybeShow :: Integer -> String
      maybeShow 1 = ""
      maybeShow n = show n
  show (OffNim (DyInt x) m) = show x ++ "*" ++ show m
  show (OffNim (DyRat x) m) = (show . numerator) x ++ "/" ++ (show . denominator) x ++ "*" ++ show m
  show g@(Game _ _)    = "{" ++ recShow depth g ++ "}"
    where
      depthCalc :: GameValue -> Int
      depthCalc (Game left right)
        |[g] <- HS.toList left, [h] <- HS.toList right = 1 + max (depthCalc g) (depthCalc h)
        |[g] <- HS.toList left                         = 1 + depthCalc g
        |[h] <- HS.toList right                        = 1 + depthCalc h
        |otherwise                                     = 1
      depthCalc _ = 0
      depth = depthCalc g
      recShow :: Int -> GameValue -> String
      recShow nSep (Game left right) = sortNShow nSep (HS.toList left) ++ replicate nSep '|' ++ sortNShow nSep (HS.toList right)
      recShow _ g = show g
      listLess :: GameValue -> GameValue -> Bool
      listLess g1 g2
        |Number x <- g1, Number y <- g2     = x < y
        |Number _ <- g1                     = True
        |Number _ <- g2                     = False
        |Nimber n <- g1, Nimber m <- g2     = n < m
        |Nimber _ <- g1                     = True
        |Nimber _ <- g2                     = False
        |OffNim x n <- g1, OffNim y m <- g2 = x < y || (x == y && n < m)
        |OffNim _ _ <- g1                   = True
        |OffNim _ _ <- g2                   = False
        |Game l1 r1 <- g1, Game l2 r2 <- g2 = HS.size l1 < HS.size l2 ||
                                              (HS.size l1 == HS.size l2 && HS.size r1 < HS.size r2) ||
                                              (HS.size l1 == HS.size l2 && HS.size r1 == HS.size r2 && show r1 < show r2)
      listGeq g1 g2 = not $ listLess g1 g2
      sortNShow :: Int -> [GameValue] -> String
      sortNShow nSep values
        |[x] <- values       = recShow (nSep-1) x
        |otherwise           = intercalate ", " $ listSnS nSep values
        where
          listSnS :: Int -> [GameValue] -> [String]
          listSnS nSep []     = []
          listSnS nSep (x:xs) = listSnS nSep (filter (`listLess` x) xs) ++ [show x] ++ listSnS nSep (filter (`listGeq` x) xs)

instance Hashable GameValue where
  hashWithSalt n g = hashWithSalt n (show g)

-- Making 'smart constructors': this allows us to simplify values before they are constructed
number :: Dyadic -> GameValue
number = Number

nimber :: Integer -> GameValue
nimber n
  |n == 0 = number 0
  |otherwise = Nimber $ abs n

offNim :: Dyadic -> Integer -> GameValue
offNim d n
  |d == 0 = nimber n
  |n == 0 = number d
  |otherwise = OffNim d $ abs n

game :: HS.HashSet GameValue -> HS.HashSet GameValue -> GameValue
game left right = canonicalForm $ Game left right

-- Getting the sided form of GameValues
toSidedForm :: GameValue -> GameValue
toSidedForm (Number n)
  |n == 0                                             = Game HS.empty HS.empty
  |DyInt _ <- n, n > 0                                = Game (HS.singleton $ number $ n - 1) HS.empty
  |DyInt _ <- n                                       = Game HS.empty (HS.singleton $ number $ n + 1)
  |DyRat r <- n, p <- numerator r, q <- denominator r = Game (HS.singleton $ number $ dyRat $ (p - 1) % q) (HS.singleton $ number $ dyRat $ (p + 1) % q)
toSidedForm (Nimber m)   = Game side side
  where
    side = HS.fromList $ [nimber n   | n <- [0..m-1]]
toSidedForm (OffNim d m) = Game side side
  where
    side = HS.fromList $ [offNim d n | n <- [0..m-1]]
toSidedForm g = g

-- Defining leftOptions and rightOptions for all game values, using toSidedForm for special games.
leftOptions :: GameValue -> HS.HashSet GameValue
leftOptions (Game left _) = left
leftOptions g = leftOptions $ toSidedForm g

rightOptions :: GameValue -> HS.HashSet GameValue
rightOptions (Game _ right) = right
rightOptions g = rightOptions $ toSidedForm g

-- Defining the none predicate for HashSets. If a predicate fails on all elements, return True. Otherwise, return False.
none :: (Foldable t) => (a -> Bool) -> t a -> Bool
none pred = foldl' (mapPred pred) True
  where
    mapPred :: (a -> Bool) -> Bool -> a -> Bool
    mapPred pred False _ = False
    mapPred pred True a
      |pred a = False
      |otherwise = True

-- Defining birthday, which functions as you may expect
birthday :: GameValue -> Integer
birthday (Number (DyInt x))   = abs x
birthday (Number (DyRat x))   = (ceiling $ abs x) + (floor $ logBase 2 $ fromInteger q)
  where
    q = denominator x
birthday (Nimber n)           = n
birthday (OffNim (DyInt x) n) = abs x + n
birthday (OffNim (DyRat x) n) = (ceiling $ abs x) + (floor $ logBase 2 $ fromInteger q) + n
  where
    q = denominator x
birthday (Game left right)    = foldl' birthStep 0 (HS.union left right)
  where
    birthStep :: Integer -> GameValue -> Integer
    birthStep n g = max n (birthday g + 1)

-- Defining the partial order relations
--
-- greater_eq is defined directly
greater_eq :: GameValue -> GameValue -> Bool
-- Comparison of numbers
(Number x)    `greater_eq` (Number y)   = x >= y
-- Comparison of nimbers
(Nimber x)    `greater_eq` (Nimber y)   = x == y
-- Comparison of nimbers to numbers
(Nimber _)    `greater_eq` (Number y)   = 0 > y
(Number x)    `greater_eq` (Nimber _)   = x > 0
-- Comparison of offset nimbers
(OffNim x m)  `greater_eq` (OffNim y n) = x > y || (x == y && m == n)
(Number x)    `greater_eq` (OffNim y _) = x > y
(OffNim x _)  `greater_eq` (Number y)   = x > y
(OffNim x _)  `greater_eq` (Nimber _)   = x > 0
(Nimber _)    `greater_eq` (OffNim y _) = 0 > y
-- Comparison of generic games
g1@(Game _ r1) `greater_eq` g2@(Game l2 _)
  |r1Size == 0, l2Size == 0 = True
  |r1Size == 0              = (none (`greater_eq` g1) l2)
  |l2Size == 0              = (none (g2 `greater_eq`) r1)
  |otherwise                = (none (g2 `greater_eq`) r1) && (none (`greater_eq` g1) l2)
  where
    r1Size = HS.size r1
    l2Size = HS.size l2
g1@(Game _ r1) `greater_eq` g2
  |r1Size == 0, Number _ <- g2 = True
  |r1Size == 0                 = (none (`greater_eq` g1) $ leftOptions g2)
  |Number _ <- g2              = (none (g2 `greater_eq`) r1)
  |otherwise                   = (none (g2 `greater_eq`) r1) && (none (`greater_eq` g1) $ leftOptions g2)
  where
    r1Size = HS.size r1
g1             `greater_eq` g2@(Game l2 _)
  |Number _ <- g1, l2Size == 0 = True
  |Number _ <- g1              = (none (`greater_eq` g1) l2)
  |l2Size == 0                 = (none (g2 `greater_eq`) $ rightOptions g1)
  |otherwise                   = (none (g2 `greater_eq`) $ rightOptions g1) && (none (`greater_eq` g1) l2)
  where
    l2Size = HS.size l2

-- The remaining relations are defined using geq
less_eq :: GameValue -> GameValue -> Bool
g1 `less_eq` g2 = g2 `greater_eq` g1

equals :: GameValue -> GameValue -> Bool
g1 `equals` g2 = g1 `greater_eq` g2 && g2 `greater_eq` g1

greater :: GameValue -> GameValue -> Bool
g1 `greater` g2 = g1 `greater_eq` g2 && (not $ g2 `greater_eq` g1)

less :: GameValue -> GameValue -> Bool
g1 `less` g2 = g2 `less` g1

confused :: GameValue -> GameValue -> Bool
g1 `confused` g2 = not $ (g1 `greater_eq` g2) || (g2 `greater_eq` g1)

-- We make GameValue an instance of Eq by setting (==) to equals
instance Eq GameValue
  where
    a == b = show a == show b || a `equals` b

-- canonicalForm turns GameValues into GameValues, obviously
canonicalForm :: GameValue -> GameValue
-- We use foldr to apply leftStep/rightStep repeatedly over the corresponding sides of the game
canonicalForm bigGame@(Game left right) = simplifyCanonicalForm $ Game (foldr leftStep HS.empty left) (foldr rightStep HS.empty right)
  where
    -- leftStep and rightStep check if an element is dominated.
    -- If it is dominated, it is skipped over
    -- If it is not dominated, it is handed over to leftRev for reversibility handling,
    -- along with the elements already present in the list which are not dominated by the element
    leftStep :: GameValue -> HS.HashSet GameValue -> HS.HashSet GameValue
    leftStep element set
      |any (`greater_eq` element) set = set
      |otherwise = leftRev element $ HS.filter (not . (element `greater_eq`)) set
    rightStep :: GameValue -> HS.HashSet GameValue -> HS.HashSet GameValue
    rightStep element set
      |any (element `greater_eq`) set = set
      |otherwise = rightRev element $ HS.filter (not . (`greater_eq` element)) set
    -- leftRev and rightRev make substitutions for reversible elements before attaching them to the list of elements
    leftRev :: GameValue -> HS.HashSet GameValue -> HS.HashSet GameValue
    leftRev element set
      |Just r <- reversibleThrough = foldr leftStep set (leftOptions r)
      |otherwise = HS.insert element set
      where
        reversibleThrough = find (bigGame `greater_eq`) $ rightOptions element
    rightRev :: GameValue -> HS.HashSet GameValue -> HS.HashSet GameValue
    rightRev element set
      |Just r <- reversibleThrough = foldr rightStep set (rightOptions r)
      |otherwise = HS.insert element set
      where
      reversibleThrough = find (`greater_eq` bigGame) $ leftOptions element
    -- numberFromSide will be used for recognizing (possibly offset) nimbers.
    -- It picks out a number from a set of games, if one is present.
    numberFromSide :: HS.HashSet GameValue -> Maybe Dyadic
    numberFromSide = foldl' numberStep Nothing
      where
        numberStep :: Maybe Dyadic -> GameValue -> Maybe Dyadic
        numberStep n@(Just _) _            = n
        numberStep _         (Number n)    = Just n
        numberStep _          _            = Nothing
    -- simplifyCanonicalForm turns special values from their two-sided canonical form into their special representation.
    simplifyCanonicalForm :: GameValue -> GameValue
    simplifyCanonicalForm g@(Game l r)
      -- Matching integers
      |[] <- HS.toList l, [] <- HS.toList r                 = number 0
      |[Number x] <- HS.toList l, [] <- HS.toList r, x >= 0 = number $ x + 1
      |[] <- HS.toList l, [Number x] <- HS.toList r, x <= 0 = number $ x - 1
      -- Matching rationals
      |[Number x] <- HS.toList l, [Number y] <- HS.toList r,  0 <= x, x < y = number $ (x + y) / 2
      |[Number x] <- HS.toList l, [Number y] <- HS.toList r,  0 >= y, y > x = number $ (x + y) / 2
      -- Matching nimbers
      |length l == length r,
       Just 0 <- fst numberTest,
       Just 0 <- snd numberTest,
       size   <- fromIntegral $ length l,
       nims   <- HS.fromList [nimber m | m <- [0..size-1]],
       l == nims, r == nims = nimber size
      -- Matching offset nimbers
      |length l == length r,
       Just n   <- fst numberTest,
       Just n   == snd numberTest,
       size     <- fromIntegral $ length l,
       nims     <- HS.fromList [offNim n m | m <- [0..size-1]],
       l == nims, r == nims = offNim n size
      -- Do nothing otherwise
      |otherwise = g
      where
        numberTest = (numberFromSide l, numberFromSide r)
    simplifyCanonicalForm g = g
-- A game in a non-sided form is unaffected by canonicalForm
canonicalForm g = g

dsum :: GameValue -> GameValue -> GameValue
-- Sum of numbers
(Number x)   `dsum` (Number y)   = number $ x + y
-- Sum of nimbers
(Nimber x)   `dsum` (Nimber y)   = nimber $ x `xor` y
-- Sum of numbers & nimbers
(Nimber x)   `dsum` (Number y)   = offNim y x
(Number x)   `dsum` (Nimber y)   = offNim x y
(OffNim x m) `dsum` (OffNim y n) = offNim (x+y) (m `xor` n)
(OffNim x m) `dsum` (Number y)   = offNim (x+y) m
(Number x)   `dsum` (OffNim y m) = offNim (x+y) m
(OffNim x m) `dsum` (Nimber n)   = offNim x (m `xor` n)
(Nimber m)   `dsum` (OffNim y n) = offNim y (m `xor` n)
-- Sum of generic games
g1 `dsum` g2 = game (HS.union (part g1 $ leftOptions g2) (part g2 $ leftOptions g1)) (HS.union (part g1 $ rightOptions g2) (part g2 $ rightOptions g1))
  where
    part :: GameValue -> HS.HashSet GameValue -> HS.HashSet GameValue
    part single set = HS.map (single `dsum`) set

neg :: GameValue -> GameValue
neg (Number x)  = number $ negate x
neg g@(Nimber _) = g
neg (OffNim  x n) = (number $ negate x) `dsum` Nimber n
neg (Game left right) = Game (HS.map neg right) (HS.map neg left)

dsub :: GameValue -> GameValue -> GameValue
g1 `dsub` g2 = g1 `dsum` neg g2

-- The calculations for Reduced Canonical Form are from the paper obtained at the link below:
-- http://library.msri.org/books/Book29/files/cali.pdf
-- Defining star cooling, i.e. G_*

starCool :: GameValue -> GameValue
starCool g@(Number _) = g
starCool (Nimber _) = number 0
starCool (OffNim x _) = number x
starCool (Game left right) = game (HS.map coolNAdd left) (HS.map coolNAdd right)
  where
    coolNAdd :: GameValue -> GameValue
    coolNAdd g = starCool g `dsum` nimber 1

-- Defining star-projection
starProj g@(Number _) = g
starProj (Nimber _) = number 0
starProj (OffNim x _) = number x
starProj (Game left right) = game (HS.map starProj left) (HS.map starProj right)

-- Defining Reduced Canonical Form
reducedCanonicalForm :: GameValue -> GameValue
reducedCanonicalForm = starProj . starCool

lstop :: GameValue -> Dyadic
lstop (Number x) = x
lstop g = foldr max f r
  where
    f: r = map rstop $ HS.toList $ leftOptions g

rstop :: GameValue -> Dyadic
rstop (Number x) = x
rstop g = foldr min f r
  where
    f: r = map lstop $ HS.toList $ rightOptions g

infGeq :: GameValue -> GameValue -> Bool
infGeq g1 g2 = (rstop $ g1 `dsum` neg g2) >= 0
