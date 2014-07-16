module Semiring where

import Data.Ix
import Data.Array
import Control.Monad
import Control.Applicative


infixl 6 <+>
infixl 7 <.>

{- Laws:
    a <+> b = b <+> a
    (a <+> b) <+> c = a <+> (b <+> c)
    a <+> zero = <zero> <+> a = a
    (a <.> b) <.> c = a <.> (b <.> c)
    a <.> one = one <.> a = a
    a <.> zero = zero <.> a = zero
    a <.> (b <+> c) = a <.> b <+> a <.> c
    (a <+> b) <.> c = a <.> c <+> b <.> c
-}
class Semiring a where
  zero :: a
  one :: a
  (<+>) :: a -> a -> a
  (<.>) :: a -> a -> a
  srsum :: [a] -> a
  srsum = foldr (<+>) zero
  srprod :: [a] -> a
  srprod = foldr (<.>) one
  
{- Laws:
    star a = one <+> a <.> star a = one <+> star a <.> a
-}
class Semiring a => StarSemiring a where 
  star :: a -> a
  star a = one <+> plus a
  plus :: a -> a
  plus a = a <.> star a

--- Bool and Num are obvious Semirings, Bool is even a simple StarSemiring

instance Semiring Bool where 
  zero = False
  one = True
  (<+>) = (||)
  (<.>) = (&&)
  
instance StarSemiring Bool where
  star _ = one
  
data Numeric a = Numeric { getNumeric :: a } deriving (Eq, Ord, Show)

instance (Num a) => Semiring (Numeric a) where 
  zero = Numeric 0
  one = Numeric 1
  (Numeric x) <+> (Numeric y) = Numeric $ x + y
  (Numeric x) <.> (Numeric y) = Numeric $ x * y

--- Describe Graphs as Semirings, by their Adjecency Matrix
  
entireRange :: (Ix i, Bounded i) => [i]
entireRange = range (minBound, maxBound)

data Edge i = i :-> i deriving (Eq, Ord, Bounded, Ix)

newtype Matrix i e = Matrix {unmatrix :: (Array (Edge i) e)}

instance (Ix i) => Functor (Matrix i) where
  fmap f (Matrix m) = Matrix (fmap f m)
  
instance (Ix i, Bounded i) => Applicative (Matrix i) where
  pure x = matrix (const x)
  Matrix f <*> Matrix x = matrix (\(i :-> j) -> (f!(i :-> j)) (x!(i :-> j)))

matrix :: (Ix i, Bounded i) => (Edge i -> e) -> Matrix i e
matrix f = Matrix . listArray (minBound, maxBound) . map f $ entireRange

instance (Ix i, Bounded i, Semiring a) => Semiring (Matrix i a) where
  zero = pure zero
  one = matrix (\(i :-> j) -> if i == j then one else zero)
  (<+>) = liftA2 (<+>)
  (Matrix x) <.> (Matrix y) = matrix build
    where
      build (i :-> j) = srsum [x!(i :-> k) <.> y!(k :-> j) | k <- entireRange]
      
{- 
   Depending on the Provided Star Semiring this calculates (e.g.): 
     * Transitive Reachability Property of Nodes
     * Shortest Path with Floyd-Warshall
     * Regular Expressions with McNaughton-Yamada
     * ...
-}
instance (Ix i, Bounded i, StarSemiring a) => StarSemiring (Matrix i a) where
  plus x = foldr f x entireRange
    where
      f k (Matrix m) = matrix build
        where
          build (i :-> j) = m!(i :-> j) <+>
                            m!(i :-> k) <.> star (m!(k :-> k)) <.> m!(k :-> j)

printMatrix :: (Ix i, Bounded i, Show a) => Matrix i a -> IO ()
printMatrix = putStrLn . showMatrix

showMatrix :: (Ix i, Bounded i, Show a) => Matrix i a -> String
showMatrix (Matrix m) = 
  unlines [concat [pad (m'!(i :-> j)) j | j <- entireRange] | i <- entireRange]
  where 
    m' = fmap show m
    lenm = fmap length m'
    len j = maximum [lenm!(i :-> j) | i <- entireRange]
    pad s j = s ++ replicate ((len j) - (length s) + 1) ' '
    
instance Show a => Show (Edge a) where
  showsPrec _ (i :-> j) = showParen True (shows i . shows j)
  

--- Integers with min and + form the Tropical Star semiring

data Tropical = Infinity | Tropical { getTropical :: !Integer } deriving (Eq, Ord)

instance Semiring Tropical where
  zero = Infinity
  one = Tropical 0
  Infinity <+> x = x
  x <+> Infinity = x
  (Tropical x) <+> (Tropical y) = Tropical $ min x y
  Infinity <.> _ = Infinity
  _ <.> Infinity = Infinity
  (Tropical x) <.> (Tropical y) = Tropical $ x + y
  
instance StarSemiring Tropical where
  star _ = one
  
instance Show Tropical where
  show (Tropical a) = show a
  show Infinity = "I"

--- [0,1] + max and * form a Semiring too  

data Reliability = Reliability Double deriving (Eq, Ord)

instance Semiring Reliability where
  zero = Reliability 0
  one = Reliability 1
  (Reliability a) <+> (Reliability b) = Reliability $ max a b
  (Reliability a) <.> (Reliability b) = Reliability $ a * b
  
instance StarSemiring Reliability where  
  star _ = one
  
instance Show Reliability where
  show (Reliability a) = show a


-- Min-Max Semiring 
  
data MinMax = Infty | MinMax Integer deriving (Eq, Ord)

instance Semiring MinMax where 
  zero = MinMax 0
  one = Infty
  _ <+> Infty = Infty
  Infty <+> _ = Infty
  (MinMax a) <+> (MinMax b) = MinMax $ max a b
  Infty <.> x = x
  x <.> Infty = x
  (MinMax a) <.> (MinMax b) = MinMax $ min a b

instance StarSemiring MinMax where
  star _ = one

instance Show MinMax where
  show (MinMax a) = show a
  show Infty = "I"

--- Example Graphs

type Graph i = Matrix i Bool

graph :: (Ix i, Bounded i) => [Edge i] -> Graph i
graph edgeList = matrix build
  where
    build i = i `elem` edgeList
    

data Node = A | B | C | D | E deriving (Eq, Ord, Bounded, Ix, Show)

exampleGraph :: Graph Node 
exampleGraph = graph [(A :-> B), (B :-> C), (C :-> D), (C :-> E), (D :-> B), (E :-> D)]

type LabeledGraph i = Matrix i (Maybe (Edge i))

labelGraph :: (Ix i, Bounded i) => Graph i -> LabeledGraph i
labelGraph m = f <$> m <*> matrix id
  where
    f True l = Just l
    f False l = Nothing
    
connect :: (Ix i) => Matrix i (Maybe a) -> Graph i
connect = fmap (maybe False (const True))


data Node2 = N1 | N2 | N3 | N4 | N5 | N6 deriving (Eq, Ord, Bounded, Ix, Show)

exampleEdgeList2 :: (Edge Node2) -> Maybe Integer
exampleEdgeList2 (i :-> j) = (lookup (i :-> j) edges) `mplus` (lookup (j :-> i) edges)
  where 
    edges = [(N1 :-> N2, 7), (N1 :-> N3, 9), (N1 :-> N6, 14),
             (N2 :-> N3,10), (N2 :-> N4,15),
             (N3 :-> N4,11), (N3 :-> N6, 2),
             (N4 :-> N5, 6),
             (N5 :-> N6, 9)]
            
data Node3 = S | AA | BB | CC | DD | T deriving (Eq, Ord, Bounded, Ix, Show)

exampleEdgeList3 :: (Edge Node3) -> Maybe Double
exampleEdgeList3 (i :-> j) = (lookup (i :-> j) edges) `mplus` (lookup (j :-> i) edges)
  where 
    edges = [(S :-> AA, 0.09),  (S :-> BB, 0.085),
             (AA :-> BB, 0.08), (AA :-> CC, 0.085),
             (BB :-> DD, 0.09), (BB :-> AA, 0.08),
             (CC :-> T, 0.08),  (CC :-> DD, 0.085),
             (DD :-> T, 0.075), (DD :-> CC, 0.085)]

exampleGraph2 :: Matrix Node2 (Maybe Integer)
exampleGraph2 = matrix exampleEdgeList2

exampleGraph3 :: Matrix Node3 (Maybe Double)
exampleGraph3 = matrix exampleEdgeList3
                                             
test1 = printMatrix . star $ exampleGraph
test3 = printMatrix . star . fmap (maybe zero Tropical) $ exampleGraph2

