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

{- Laws:
    a <+> a = a
    a <.> x <+> x = x ==> star a <.> x <+> x = x
    x <.> a <+> x = x ==> x <.> star a <+> x = x
-}
class StarSemiring a => KleeneAlgebra a where

--- Bool and Num are obvious Semirings, Bool is even a simple StarSemiring

instance Semiring Bool where 
  zero = False
  one = True
  (<+>) = (||)
  (<.>) = (&&)
  
instance StarSemiring Bool where
  star _ = one
  
instance KleeneAlgebra Bool where 
  
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

instance (Ix i, Bounded i, KleeneAlgebra a) => KleeneAlgebra (Matrix i a) where

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
  






--- Regular Expressions obviously form another Starsemiring (if not the mother of them)

data StarSemiringExpression a 
  =   Var a
    | Or (StarSemiringExpression a) (StarSemiringExpression a)
    | Seq (StarSemiringExpression a) (StarSemiringExpression a)
    | Star (StarSemiringExpression a)
    | Empty
    | None
      
newtype RE a = RE (StarSemiringExpression a)

re :: a -> RE a
re = RE . Var

instance Semiring (RE a) where
  zero = RE None
  one = RE Empty
  (RE None) <+> x = x
  x <+> (RE None) = x
  (RE Empty) <+> (RE (Star x)) = RE (Star x)
  (RE (Star x)) <+> (RE Empty) = RE (Star x)
  (RE x) <+> (RE y) = RE $ x `Or` y
  x <.> (RE Empty) = x
  (RE Empty) <.> x = x
  (RE None) <.> _ = RE None
  _ <.> (RE None) = RE None
  (RE x) <.> (RE y) = RE $ x `Seq` y
  
instance StarSemiring (RE a) where
  star (RE None) = RE Empty
  star (RE Empty) = RE Empty
  star (RE (Star x)) = star (RE x)
  star (RE x) = RE (Star x)
  
instance KleeneAlgebra (RE a) where

instance Show a => Show (StarSemiringExpression a) where 
  showsPrec d (Var a) = showParen (d > 10) (shows a)
  showsPrec d Empty = showParen (d > 10) (showString "e")
  showsPrec d None = showParen (d > 10) (showString "0")
  showsPrec d (Star x) = showParen (d > 9) (showsPrec 9 x . showString "*")
  showsPrec d (x `Or` y) = showParen (d > 6) showStr
    where
      showStr = showsPrec 6 x . showString "|" . showsPrec 6 y
  showsPrec d (x `Seq` y) = showParen (d > 7) showStr
    where
      showStr = showsPrec 7 x . showsPrec 7 y
      
instance Show a => Show (RE a) where
  showsPrec d (RE x) = showsPrec d x
  
instance Show Tropical where
  show (Tropical a) = show a
  show Infinity = "I"
  


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
  
instance KleeneAlgebra Tropical where  


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

--- Combined Semiring from Tropical and Regular Expression
    
data BestPath a x = BestPath a x

instance Functor (BestPath a) where
  fmap f (BestPath a x) = BestPath a (f x)
  
extract :: BestPath a x -> x
extract (BestPath _ x) = x


instance (Semiring a, Ord a, Semiring x) => Semiring (BestPath a x) where
  zero = BestPath zero zero
  one = BestPath one one
  (BestPath a x) <.> (BestPath b y) = BestPath (a <.> b) (x <.> y)
  (BestPath a x) <+> (BestPath b y) | ((a <+> b == a) && (not (b == a <+> b))) = (BestPath a x) 
                                    | ((a <+> b == b) && (not (a == a <+> b)))= (BestPath b y)
                                    | otherwise = BestPath (a <+> b) (x <+> y)
  
instance (StarSemiring a, Ord a, StarSemiring x) => StarSemiring (BestPath a x) where
  star (BestPath a x) | a == one = BestPath one (star x)
                      | otherwise = BestPath one one
  
instance (Show a, Show x) => Show (BestPath a x) where
  show (BestPath a x) = show x ++ "[" ++ show a ++ "]"



evalRE :: (KleeneAlgebra a) => (l -> a) -> RE l -> a
evalRE f (RE None) = zero
evalRE f (RE Empty) = one
evalRE f (RE (Var a)) = f a
evalRE f (RE (Star x)) = star (evalRE f (RE x))
evalRE f (RE (x `Or` y)) = (evalRE f (RE x)) <+> (evalRE f (RE y))
evalRE f (RE (x `Seq` y)) = (evalRE f (RE x)) <.> (evalRE f (RE y))

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

reGraph :: (Ix i) => Matrix i (Maybe a) -> Matrix i (RE a)
reGraph = fmap (maybe zero re)


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
                                             
annotate :: (Ix i, Bounded i, Semiring x) =>
            ((Edge i) -> x) -> Matrix i (Maybe Integer) -> Matrix i (BestPath Tropical x)
annotate f m = go <$> m <*> labelGraph (connect m)
  where
    go v e = BestPath (maybe zero Tropical v) (maybe zero f e)
             


test1 = printMatrix . star $ exampleGraph
test2 = printMatrix . star . reGraph . labelGraph $ exampleGraph
test3 = printMatrix . star . fmap (maybe zero Tropical) $ exampleGraph2
test4 = printMatrix . star . annotate re $ exampleGraph2

test5 = printMatrix . fmap (evalRE Tropical) . star . reGraph $ exampleGraph2