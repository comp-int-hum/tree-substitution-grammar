{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.TSG.Statistics where

import Data.Text (Text)
import Data.Void (Void)
import Data.Tree (Tree(..))
import Data.Set (Set(..))
import qualified Data.Set as Set
import Control.Lens ((^.), (.~), (&), (<&>), set, view, makeLenses, makeFields, (%~))
import Control.Monad.Identity (Identity(..))
import Control.Monad.RWS (RWS(..))
import Text.Megaparsec (ParsecT, runParserT, some, eof, (<|>), sepBy, between, many, sepBy1, choice, try, satisfy)
import Text.Megaparsec.Char (char, space, spaceChar, alphaNumChar, punctuationChar, space1)

data Statistics = Statistics { _productions :: Set String
                   }
makeLenses ''Statistics

empty = Statistics Set.empty
