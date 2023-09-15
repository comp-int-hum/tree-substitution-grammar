module Text.TSG.SamplingState where

import Data.Text (Text)
import Data.Void (Void)
import Data.Tree (Tree(..))
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Control.Lens ((^.), (.~), (&), (<&>), set, view, makeLenses, makeFields, (%~))


data SamplingState = SamplingState { _nonterminalCounts :: Map String Int
                                   } deriving (Show)

makeLenses ''SamplingState

empty = SamplingState Map.empty
