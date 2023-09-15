{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.TSG.PCFG where

import Data.Text (Text)
import Data.Void (Void)
import Data.Tree (Tree(..))
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Control.Lens ((^.), (.~), (&), (<&>), set, view, makeLenses, makeFields, (%~))
import Data.Tree (Tree(..))
import Text.TSG.TreeBank (TreeBank)


type PCFG = Map (String, [String]) Double
type RuleCounts = Map (String, [String]) Int

fromTreebank :: [Tree String] -> PCFG
fromTreebank tb = fromCounts $ foldr inc Map.empty (concat $ map flatten tb)

fromCounts :: RuleCounts -> PCFG
fromCounts rc = Map.mapWithKey (\(k, _) v -> log $ (fromIntegral v) / (fromIntegral $ nonTerminalCounts Map.! k)) rc
  where
    nonTerminalCounts = Map.fromListWith (\a b -> a + b) $ (map (\((r, _), c) -> (r, c)) . Map.toList) rc

flatten :: Tree String -> [Tree String]

flatten (Node label []) = []

flatten (Node label children) = concat $ [[Node label shallowChildren]] ++ (map flatten children)
  where
    shallowChildren = [Node l [] | Node l _ <- children] 

inc :: Tree String -> RuleCounts -> RuleCounts
inc n p = Map.insert (rootLabel n, children) (cur + 1) p
  where
    children = [rootLabel x | x <- subForest n]
    cur = Map.findWithDefault 0 (rootLabel n, children) p
