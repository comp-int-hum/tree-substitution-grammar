module Text.TSG.Gibbs where

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
