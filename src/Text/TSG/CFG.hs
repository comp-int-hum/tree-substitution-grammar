module Text.TSG.CFG where
import Prelude hiding (log)
import qualified Prelude as Prelude
import Data.Text (Text)
import Data.Void (Void)
import Data.Maybe (catMaybes, fromJust)
import Data.Tree (Tree(..))
import Data.Either (lefts, rights)
import Data.List (mapAccumR, partition, nub, intercalate)
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Control.Lens ((^.), (.~), (&), (<&>), set, view, makeLenses, makeFields, (%~), at, non, (%=), contains, (?~))
import Data.Tree (Tree(..))
import Text.TSG.TreeBank (TreeBank)
import Debug.Trace (traceShowId)
import Control.DeepSeq (deepseq)
import Data.Tuple (swap)
import Data.Monoid (Sum(..))

logarithm = Prelude.log

data Symbol nt t = Start { _nonterm :: nt } | NonTerminal { _nonterm :: nt, _der :: Int } | Terminal { _term :: t } deriving (Ord, Eq, Show)



type Rules nt t a = Map nt (Map [Either nt t] a)
--type Rules nt t a = Map (Symbol nt t) (Map [Symbol nt t] a)
data CFG nt t a = CFG { _rules :: Rules nt t a
                      , _startSymbols :: Set nt
                      } deriving (Eq)


makeLenses ''Symbol
makeLenses ''CFG

instance (Show nt, Show t, Show a) => Show (CFG nt t a) where
  show c = intercalate "\n" lines
    where
      sline = "Start symbols: " ++ ((show . Set.toList) $ c ^. startSymbols)
      rlines = [intercalate " " ([show lhs, "-->"] ++ (map (\case Left v -> show v; Right v -> show v) rhs) ++ [show v])| (lhs, rhs, v) <- toRuleList c]
      lines = [sline] ++ rlines

empty = CFG { _rules=Map.empty
            , _startSymbols=Set.empty
            }

exampleGrammar = fromRuleList (Set.singleton "S") [ ("S", map Left ["NP", "VP"], 8)
                                                  , ("S", map Left ["S", "CC", "S"], 2)
                                                  , ("NP", map Left ["NN"], 2)
                                                  , ("NP", map Left ["DT", "NN"], 4)
                                                  , ("NP", map Left ["NP", "PP"], 2)
                                                  , ("NP", [Left "NP", Right "and", Left "NP"], 2)
                                                  , ("VP", map Left ["VB"], 3)
                                                  , ("VP", map Left ["VB", "NP"], 3)
                                                  , ("VP", map Left ["VB", "NP", "NP"], 1)
                                                  , ("VP", map Left ["VP", "PP"], 3)
                                                  , ("PP", map Left ["PR", "NP"], 1)
                                                  , ("PR", map Left ["P"], 1)
                                                  , ("NN", map Left ["N"], 1)
                                                  , ("VB", map Left ["V"], 1)
                                                  , ("N", [Right "John"], 33)
                                                  , ("N", [Right "candy"], 33)
                                                  , ("N", [Right "gusto"], 33)
                                                  , ("V", [Right "eats"], 1)
                                                  , ("P", [Right "with"], 1)
                                                  ] :: CFG String String Int



toRuleList cfg = concat $ [[(lhs, rhs, v) | (rhs, v) <- Map.toList rhss] | (lhs, rhss) <- Map.toList $ cfg ^. rules]

fromRuleList ss rs = CFG rm ss
  where
    rm = (Map.fromListWith (\a b -> Map.union a b) . map (\(lhs, rhs, v) -> (lhs, Map.singleton rhs v))) rs

terminals :: (Ord t) => CFG nt t a -> Set t
terminals cfg = Set.fromList $ (catMaybes . map (\case {Left _ -> Nothing; Right s -> Just s}) . concat . concat . map Map.keys . Map.elems) $ cfg ^. rules

nonTerminals :: (Ord nt) => CFG nt t a -> Set nt
nonTerminals cfg = (Set.fromList . Map.keys) $ cfg ^. rules

rightReplaceWith :: (Ord nt) => CFG nt t a -> Either nt t -> Either nt t -> CFG nt t a
rightReplaceWith cfg a b = cfg

fromTreebank :: (Monoid nt, Show nt, Ord nt, Ord t) => [Tree (Either nt t)] -> CFG nt t Int
fromTreebank tb = empty & rules .~ rs & startSymbols .~ starts
  where
    rs = (ruleCountsFromRuleSequence . ruleSequenceFromTreebank) tb
    starts = (Set.fromList . lefts) $ map rootLabel tb
--     rs = (ruleProbabilitiesFromRuleCounts . toCNF . ruleCountsFromRuleSequence . ruleSequenceFromTreebank) tb

toPCFG :: CFG nt t Int -> CFG nt t (Sum Double)
toPCFG cfg = cfg & rules .~ rules'
  where
    rules' = Map.map normalizeLHS (cfg^.rules)

normalizeLHS :: Map [Either nt t] Int -> Map [Either nt t] (Sum Double)
normalizeLHS rhss = rhss'
  where
    total = (fromIntegral . sum . Map.elems) rhss
    rhss' = Map.map (\c -> Sum $ logarithm $ fromIntegral c / total) rhss

ruleCountsFromRuleSequence :: (Ord nt, Ord t) => [(nt, [Either nt t])] -> Rules nt t Int
ruleCountsFromRuleSequence rs = foldr incrementRule Map.empty rs

incrementRule :: (Ord nt, Ord t) => (nt, [Either nt t]) -> Rules nt t Int -> Rules nt t Int
incrementRule (nt, rhs) rc = rc & at nt . non mempty . at rhs . non 0 %~ (\c -> c+1)

ruleSequenceFromTreebank :: (Ord nt, Ord t) => [Tree (Either nt t)] -> [(nt, [Either nt t])]
ruleSequenceFromTreebank tb = concat $ map ruleSequenceFromTree tb

ruleSequenceFromTree :: (Ord nt, Ord t) => Tree (Either nt t) -> [(nt, [Either nt t])]
ruleSequenceFromTree (Node t []) = []
ruleSequenceFromTree (Node (Left nt) xs) = (nt, map rootLabel xs) : rest
  where
    rest = concat $ map ruleSequenceFromTree xs

toCNF :: (Monoid nt, Ord nt, Show nt, Show t, Ord t) => nt -> nt -> nt -> CFG nt t Int -> CFG nt t Int
toCNF sb b m cfg = cfg''
  where
    starts = cfg ^. startSymbols
    cfg' = (unitary b m . epsilon b m . binarize b m . nonsolitary b m . start sb m) cfg
    starts' = cfg' ^. startSymbols
    rs = toRuleList $ cfg'
    rhnts = Set.fromList $ (concat . map (\(lhs, rhs, v) -> lefts rhs)) rs
    rs' = filter (\(lhs, rhs, v) -> lhs `Set.member` starts' || lhs `Set.member` rhnts) rs
    
    cfg'' = fromRuleList starts' rs'

start, binarize, epsilon, unitary, nonsolitary :: (Show t, Show nt, Monoid nt, Ord nt, Show nt, Ord t) => nt -> nt -> CFG nt t Int -> CFG nt t Int


-- create new start symbol with appropriate productions/probabilities
start b s cfg = cfg & startSymbols .~ Set.singleton start' & rules . at start' ?~  startProds
  where
    start':[] = newSyms 1 cfg b s
    oldStart = cfg ^. startSymbols
    startProds = Map.fromList [([Left lhs], sum . Map.elems $ rhss) | (lhs, rhss) <- Map.toList $ cfg ^. rules, lhs `Set.member` oldStart]


-- rules with non-solitary terminals
nonsolitary b s cfg = cfg'
  where
    ss = cfg ^. startSymbols
    terms = (Set.toList . Set.fromList . rights . concat) [rhs | r@(lhs, rhs, v) <- toRuleList cfg, length rhs > 1 && length (rights rhs) > 0]
    ns = newSyms (length terms) cfg b s
    newRules = [(nt, [Right t], 1) | (nt, t) <- zip ns terms]
    newRuleMap = Map.fromList [(Right t, Left nt) | (nt, t) <- zip ns terms]    
    oldRules = [(lhs, [Map.findWithDefault o o newRuleMap | o <- rhs], v) | (lhs, rhs, v) <- toRuleList cfg]
    cfg' = fromRuleList ss (newRules ++ oldRules)
    

-- rules with more than two non-terminals
binarize b s cfg = cfg'
  where
    ss = cfg ^. startSymbols
    rs = Set.fromList $ filter (\(_, x, _) -> length x > 2) . toRuleList $ cfg
    oldRules = [r | r <- toRuleList cfg, not $ r `Set.member` rs] --Map.empty --cfg ^. rules
    ns = map (\(lhs, rhs, a) -> length rhs - 2) (Set.toList rs)
    n = sum ns
    newNonTerms = newSyms n cfg b s
    (_, nts) = mapAccumR (\nts' n' -> swap $ splitAt n' nts') newNonTerms ns
    newRules = concat $ map binarizeRule (zip nts (Set.toList rs))
    cfg' = fromRuleList ss (newRules ++ oldRules)


binarizeRule :: (Show a, Monoid nt, Ord nt, Ord t, Eq a, Show nt, Show t) => ([nt], (nt, [Either nt t], a)) -> [(nt, [Either nt t], a)]
binarizeRule (nts, r@(lhs, rhs, v)) = newRules
  where
    l = length rhs
    nts' = map Left nts
    lt = (reverse . take 2 . reverse) rhs
    finalRule = (last nts, lt, v)
    newRules = finalRule:[(lhs, [a,b], v) | (lhs, a, b) <- zip3 (lhs:(init nts)) (init rhs) (nts' ++ [last rhs])]


-- non-terminals with epsilon productions (TODO/not immediately relevant)
epsilon b s cfg = cfg
  where
    nullable = []

-- rules with unitary non-terminals
-- unitary b s cfg = cfg --fromRuleList ss (go remove leave) --undefined -- go toReplace
--   where
--     ss = cfg ^. startSymbols
--     (remove, leave) = partition (\(lhs, rhs, _) -> (length . lefts) rhs == 1 && (length . rights) rhs == 0 && rhs /= [Left lhs]) . toRuleList $ cfg
--     go unitaries acc = case unitaries of
--       [] -> acc
--       (lhs, rhs, v):rest -> traceShowId (length rest, lhs, rhs) `seq` go ((Set.toList . Set.fromList) $ rest ++ remove') ((Set.toList . Set.fromList) $ acc ++ leave')
--       --(lhs, rhs, v):rest -> traceShowId (lhs, rhs, remove', leave') `seq` go ((Set.toList . Set.fromList) $ rest ++ remove') ((Set.toList . Set.fromList) $ acc ++ leave')
--         where
--           Left r = head rhs
--           newRules = [(lhs, rhs', v) | (rhs', v') <- (Map.toList . (\d -> d Map.! r)) (cfg ^. rules), rhs' /= [Left lhs]]          
--           (remove', leave') = partition (\(_, x, _) -> (length . lefts) x == 1 && (length . rights) x == 0) $ newRules
--           remove'' = [] --traceShowId (lhs, rhs, r, length leave') `seq` []

unitary b s cfg = go remove cfg' Set.empty --cfg --fromRuleList ss (go remove leave) --undefined -- go toReplace
  where
    ss = cfg ^. startSymbols
    
    -- remove rules of the form "A -> A"
    rs = [(lhs, rhs, v) | (lhs, rhs, v) <- toRuleList cfg, [Left lhs] /= rhs]
    cfg' = fromRuleList ss rs

    -- split into unitary/non-unitary rules and keep the former
    remove = (Set.fromList . fst . partition (\(lhs, rhs, _) -> (length . lefts) rhs == 1) . toRuleList) cfg'
    
    go unitaries cfg'' removed = case Set.toList unitaries of
      [] -> cfg''
      (lhs, rhs, v):rest -> go (Set.fromList $ rest ++ remove') cfg''' removed
      --(lhs, rhs, v):rest -> traceShowId (length rest, (sum . concat . map Map.elems . Map.elems) combinedRules) `seq` go (Set.fromList $ rest ++ remove') cfg''' removed
      --((Set.toList . Set.fromList) $ acc ++ leave')
      --(lhs, rhs, v):rest -> traceShowId (lhs, rhs, remove', leave') `seq` go ((Set.toList . Set.fromList) $ rest ++ remove') ((Set.toList . Set.fromList) $ acc ++ leave')
        where
          Left r = head rhs
          Just rules' = cfg'' ^. rules ^. at r          
          newRules = [(lhs, rhs', v') | (rhs', v') <- Map.toList rules']
          --newRules = [(lhs, rhs', v) | (rhs', v') <- (Map.toList . (\d -> Map.findWithDefault Map.empty r d)) undefined]
          (remove', leave') = partition (\(_, x, _) -> (length . lefts) x == 1 && (length . rights) x == 0) $ newRules
          newRules' = (fromRuleList ss leave') ^. rules
          remove'' = Set.fromList remove'
          --combinedRules = (Set.toList . Set.fromList) [r | r <- leave' ++ (toRuleList cfg'), not $ r `Set.member` remove'']
          combinedRules = (Map.unionWith Map.union newRules' (cfg'' ^. rules)) & at lhs . non Map.empty . at rhs .~ Nothing

          --combinedRules' = fromRuleList ss $ [x | x <- toRuleList combinedRules, not $ x `Set.member` remove'']
          cfg''' = cfg'' & rules .~ combinedRules
          

-- handleOneUnitary :: (Ord nt, Ord t, Eq a, Show nt, Show t, Show a) => CFG nt t a -> (nt, [Either nt t], a) -> CFG nt t a
-- handleOneUnitary cfg (lhs, rhs, a) = cfg & rules .~ rs'' -- (rs & at lhs . non empty . at rhs .~ Nothing)
--   where
--     rs = cfg ^. rules
--     rs' = rs & at lhs . non mempty . at rhs .~ Nothing -- remove unitary rule
--     Left ds = head rhs
--     Just toConnect = Map.toList <$> rs ^. at ds
--     (rs'', _) = if ds == lhs then (rs', undefined) else mapAccumR (\rs''' (rhs', v) -> (rs''' & at lhs . non mempty . at rhs' .~ Just v, Nothing)) rs' toConnect


-- generate n new unique non-terminal symbols for the given CFG, starting with the base b and using suffix-element s
newSyms :: (Monoid nt, Ord nt, Show nt, Ord t, Eq a, Show nt, Show t, Show a) => Int -> CFG nt t a -> nt -> nt -> [nt]
newSyms n cfg b s = Set.toList $ go b Set.empty
  where
    go b' pg = case (col, suf) of
                 (_, True) -> pg
                 (True, _) -> go (b' <> s) pg
                 (False, _) -> go (b' <> s) pg'
      where
        col = b' `Map.member` (cfg ^. rules) || b' `Set.member` pg
        suf = Set.size pg == n
        pg' = b' `Set.insert` pg

