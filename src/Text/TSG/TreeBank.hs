module Text.TSG.TreeBank where

import Data.Text (Text)
import Data.Void (Void)
import Data.Tree (Tree(..))
import Data.Map (Map(..))
import qualified Data.Map as Map
import Control.Monad.Identity (Identity(..), runIdentity)
import Control.Monad.RWS (RWS(..), runRWST)
import Control.Monad.State (State(..), runStateT, MonadState(..))
import Control.Lens
import Text.Megaparsec (ParsecT, runParserT, some, eof, (<|>), sepBy, between, many, sepBy1, choice, try, satisfy, MonadParsec, Token(..))
import Text.Megaparsec.Char (char, space, spaceChar, alphaNumChar, punctuationChar, space1)
import Text.TSG.SamplingState (SamplingState, empty, nonterminalCounts)

--type Parser = ParsecT Void Text Identity --(RWS Void Void State)
type Parser = ParsecT Void Text (State SamplingState)
type TreeBank = [Tree (Either String String)]

treebank :: (MonadParsec e s m, Token s ~ Char, MonadState SamplingState m) => m TreeBank
treebank = between space (space >> eof) $ sepBy tree space

tree :: (MonadParsec e s m, Token s ~ Char, MonadState SamplingState m) => m (Tree (Either String String))
tree = between oParen cParen subtree

subtree :: (MonadParsec e s m, Token s ~ Char, MonadState SamplingState m) => m (Tree (Either String String))
subtree = internal <|> leaf

leaf :: (MonadParsec e s m, Token s ~ Char) => m (Tree (Either String String))
leaf = token >>= (\l -> return $ Node (Right l) [])

internal :: (MonadParsec e s m, Token s ~ Char, MonadState SamplingState m) => m (Tree (Either String String))
internal = between oParen cParen $ do
  label <- token
  s <- get
  let m = s ^. nonterminalCounts
      cur = Map.findWithDefault 0 label m
  put (s & nonterminalCounts .~ (Map.insert label (cur + 1) m))
  subtrees <- sepBy subtree space
  return $ Node (Left label) subtrees

token :: (MonadParsec e s m, Token s ~ Char) => m String
token = between space space $ some (satisfy (\c -> not $ c `elem` ['(', ')', '\t', ' ', '\n']))

oParen :: (MonadParsec e s m, Token s ~ Char) => m ()
oParen = space >> (char '(') >> space

cParen :: (MonadParsec e s m, Token s ~ Char) => m ()
cParen = space >> (char ')') >> space

--load :: Text -> IO [Tree String]
load prds = do
  let (Right tb, stats) = runIdentity $ runStateT (runParserT (treebank :: Parser TreeBank) "test" prds) empty
  return tb
