module Text.TSG.Chart where

import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.List (intercalate)
import Debug.Trace (traceShowId)
import Text.Printf (printf, PrintfArg)
import Control.Lens ((^.), (.~), (&), (<&>), set, view, makeLenses, makeFields, (%~), at, non, (%=), contains, (?~), traverseOf, traverse, each, sequenceOf, traversed, (^..), itoList, to)

data Chart nt t a = Chart { _cells :: Map Index (Cell nt t a) -- (Map nt (Cell nt t a))
                          , _terminals :: [t]
                          , _starts :: Set nt
                          } deriving (Eq)

emptyChart = Chart Map.empty [] Set.empty

data Index = Index { _start :: Int
                   , _span :: Int
                   } deriving (Eq, Show, Ord)

nullIndex = Index undefined undefined

data Cell nt t a = Cell { _possible :: Map nt (Rule nt t a)
                        } deriving (Eq, Show, Ord)

-- data Cell nt t a = Cell { _back :: Either (Pointer nt) t
--                         , _score :: a
--                         } deriving (Eq, Show)

data Rule nt t a = Rule { _pointer :: Either (Pointer nt t a) t
                        , _score :: a
                        } deriving (Eq, Ord)

instance (Ord nt, Show nt, Show t, Show a) => Show (Rule nt t a) where
  show (Rule ptr s) = show ptr

data Pointer nt t a = Pointer { _partition :: Int
                              , _left :: nt
                              , _right :: nt
                              } deriving (Eq, Show, Ord)

makeLenses ''Pointer
makeLenses ''Cell
makeLenses ''Rule
makeLenses ''Index
makeLenses ''Chart


bestStart chart = (nt, p)
  where
    n = length $ chart ^. terminals
    idx = Index 1 n
    ss = chart ^. starts
    rs = [x | x <- Map.toList $ chart ^. cells . at idx . traverse . possible, fst x `Set.member` ss]
    (nt, r) = head rs
    p = r ^. score


instance (Ord nt, Show nt, Show t, Show a, PrintfArg t, PrintfArg nt) => Show (Chart nt t a) where
  show chart = intercalate "\n\n" (initial : (rows ++ [final]))
    where

      ts = chart ^. terminals
      cs = chart ^. cells ^.. traverse . possible & map Map.size
      cs' = chart ^. cells ^.. traverse . possible & (map length . map show . map fst . concat . map Map.toList)
      sep = replicate 6 ' '
      maxPoss = maximum cs
      maxSym = (maximum $ cs' ++ (map (length . show) ts)) - 2
      blank = replicate maxSym ' '
          
      printT :: (PrintfArg v) => v -> String
      printT t = printf "%*s" maxSym t

      oneCellPart r c i = if i > length poss then replicate maxSym ' ' else printT (fst $ poss !! (i - 1))
        where
          idx = Index r c
          poss = (Map.toList $ chart ^. cells . at idx . traverse . possible)

      initial = printf "%40s" ("Span length" :: String)
      rows = map (\(r, t) -> intercalate "\n" [intercalate sep ([if i == 1 then printT t else blank] ++ [oneCellPart r c i | c <- [1..length ts]]) | i <- [1..maxPoss]]) (zip [1..] ts)
      
      final = intercalate sep $ [replicate maxSym ' '] ++ (map (printf "%*s" maxSym) ts)


