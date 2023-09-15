module Text.TSG.CKY where

import Prelude hiding (log, span)
import Data.Text (Text)
import Data.Void (Void)
import Data.Tree (Tree(..))
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Control.Lens ((^.), (.~), (&), (<&>), set, view, makeLenses, makeFields, (%~), at, non, (^?), _Just, traverse, to)
import Control.Monad (foldM, mapM, liftM)
import Data.Tree (Tree(..))
import Text.TSG.TreeBank (TreeBank)
import Text.TSG.CFG (CFG, terminals, nonTerminals, toRuleList, rules, startSymbols, fromRuleList)
import Debug.Trace (traceShowId)
import Data.List (mapAccumR, sortOn)
import Data.Either (lefts, rights)
import Text.TSG.Log (log, Log)
import Text.Printf (PrintfArg)
import Text.TSG.Chart (Chart(..), Pointer(..), Rule(..), Cell(..), cells, partition, left, right, pointer, score, possible, start, span, bestStart, emptyChart, nullIndex)
import qualified Text.TSG.Chart as Chart
import Effectful (Eff, (:>))


exampleSentence = ["John", "eats", "candy", "with", "gusto"]


parse :: (nt ~ t, PrintfArg t, Log :> es, Show t, Ord nt, Ord t, Show nt, Eq t, Monoid a, Ord a, Show a) => CFG nt t a -> [t] -> Eff es (a, Tree (Either nt t))
parse pcfg ss = do
  log 3 $ "Parsing sentence: " ++ (show ss)
  chart <- initialChart pcfg ss
  log 3 $ "Initial chart state:\n" ++ (show chart)
  chart' <- foldM (forLengthOfSpan pcfg n) chart [2..n]
  log 3 $ "Final chart state:\n" ++ (show chart')
  (prob, tree) <- likeliestParse pcfg chart' ss
  pure (prob, tree)
    where
      terms = terminals pcfg
      nonTerms = nonTerminals pcfg
      n = length ss
      r = length nonTerms


initialChart :: (nt ~ t, Log :> es, Show nt, Show t, Ord nt, Eq t, Ord t, Monoid a) => CFG nt t a -> [t] -> Eff es (Chart nt t a)
initialChart cfg ts = do
  log 3 "  Building initial chart from terminal productions"
  pure $ emptyChart & Chart.terminals .~ ts & Chart.cells .~ cs & Chart.starts .~ ss
    where
      rs = toRuleList cfg
      ss = cfg ^. startSymbols
      termProds = Map.fromList $ [(t, (Cell . Map.fromList) $ [(lhs, (Rule (Right t) v)) | (lhs, rhs, v) <- rs, rhs == [Right t]]) | t <- (Set.toList . Set.fromList) ts]
      cs = Map.fromList $ [((nullIndex & start .~ i & span .~ i), termProds Map.! t) | (i, t) <- zip [1..length ts] ts]


likeliestParse :: (Log :> es, Show nt, Ord nt, nt ~ t, Ord a, Monoid a, Show a, Show t) => CFG nt t a -> Chart nt t a -> [t] -> Eff es (a, Tree (Either nt t))
likeliestParse pcfg chart ss = do
  log 3 "Reconstructing best parse tree:"
  let n = length ss
      starts = pcfg ^. startSymbols
      depth = 0
      cs = chart ^. cells
      (nt, bestScore) = chart ^. to bestStart
      --(nt, Rule (Left (Pointer partition left right)) bestScore):_ = undefined --[x | x <- Map.toList $ cs Map.! (Index 1 n), fst x `Set.member` starts]
  trs <- reconstructTree chart ss n 1 nt
  pure (bestScore, head trs)


reconstructTree :: (Monoid a, Log :> es, Ord nt, Show nt, Ord a, Show t, Show a) => Chart nt t a -> [t] -> Int -> Int -> nt -> Eff es [Tree (Either nt t)]
reconstructTree chart ss l s nt = do
  
  let n = length ss
      idx = nullIndex & start .~ s & span .~ (l + s - 1)
      poss = chart ^. cells . at idx . traverse . possible
      Just (Rule back _) = poss ^. at nt
  log 3 $ "  Considering node at " ++ (show idx)
  case back of
    Right t' -> do
      log 3 $ "    Preterminal"
      pure [Node (Left nt) [Node (Right t') []]]
    Left (Pointer p ntA ntB) -> do
      log 3 $ "    Non-terminal"
      let lA = p
          sA = s
          lB = l - p
          sB = s + p
      trA <- reconstructTree chart ss lA sA ntA
      trB <- reconstructTree chart ss lB sB ntB
      pure [Node (Left nt) (trA ++ trB)]


forLengthOfSpan :: (Log :> es, Ord nt, Show nt, Show a, Show t, Ord a, Monoid a) => CFG nt t a -> Int -> Chart nt t a -> Int -> Eff es (Chart nt t a)
forLengthOfSpan pcfg n !chart l = do
  log 3 $ "  Considering spans of length " ++ (show l)
  chart' <- foldM (forStartOfSpan pcfg n l) chart [1..n-l+1]
  pure chart'


forStartOfSpan :: (Log :> es, Ord nt, Show t, Show a, Show nt, Ord a, Monoid a) => CFG nt t a -> Int -> Int -> Chart nt t a -> Int -> Eff es (Chart nt t a)
forStartOfSpan pcfg n l chart s = do
  log 3 $ "    Considering spans starting at position " ++ (show s)
  options <- mapM (forPartitionOfSpan pcfg n l s chart) [1..l-1]
  let options' = (sortOn snd . concat) $ options
      vals = Just $ Cell $ Map.fromList [(lhs, (Rule (Left $ Pointer p rhA rhB) prob)) | ((lhs, p, rhA, rhB), prob) <- options']
      idx = nullIndex & start .~ s & span .~ (s + l - 1)
  pure $ chart & cells . at idx .~ vals


forPartitionOfSpan :: (Log :> es, Ord nt, Show t, Show a, Show nt, Ord a, Monoid a) => CFG nt t a -> Int -> Int -> Int -> Chart nt t a -> Int -> Eff es [((nt, Int, nt, nt), a)]
forPartitionOfSpan pcfg n l s chart p = do
  log 3 $ "      Considering partition at position " ++ (show p)
  let idxA = nullIndex & start .~ s & span .~ (s + p - 1)
      idxB = nullIndex & start .~ (s + p) & span .~ (s + l - 1)
      aPoss = chart ^. cells . at idxA . traverse . possible -- Map.! (s, s + p - 1)
      bPoss = chart ^. cells . at idxB . traverse . possible -- chart Map.! (s + p, s + l - 1)
      options = [((lhs, p, rhA, rhB), v <> (aPoss ^. at rhA . traverse . score) <> (bPoss ^. at rhB . traverse . score)) | (lhs, rhs@((Left rhA):(Left rhB):[]), v) <- toRuleList pcfg, length rhs == 2 && rhA `Map.member` aPoss && rhB `Map.member` bPoss]
  log 3 $ "      Viable splits: "
  log 3 $ "        LHS: " ++ (show aPoss)
  log 3 $ "        RHS: " ++ (show bPoss)
  log 3 $ "      Viable rules: " ++ (show options)
  pure options
