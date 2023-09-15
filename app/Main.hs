module Main where

import Prelude hiding (log)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Options.Generic (Generic, ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<?>)(..))
import qualified Data.Text.IO as Text
import Data.Tree (Tree(..), drawTree)
import qualified Data.Set as Set
import Control.Monad (liftM2)
import Control.Lens ((^.), (.~), (&), (<&>), set, view, makeLenses, makeFields, (%~), at, (?~))
import Effectful (runEff, (:>), Eff(..), liftIO)
import Text.TSG.TreeBank (TreeBank)
import qualified Text.TSG.TreeBank as TreeBank
import Text.TSG.CFG (CFG, rules, terminals, nonTerminals, startSymbols, toRuleList, fromRuleList, exampleGrammar)
import qualified Text.TSG.CFG as CFG
import Text.TSG.CKY (parse, exampleSentence)
import Text.TSG.Log (runLog, defaultLogger, Log(..), log, level)
import Data.Semigroup (getSum)
import System.IO (IOMode(..), openFile, hPrint, Handle, hClose)

data Args w = Train { trainingInput :: w ::: Maybe String <?> "Training data input"
                    , grammarOutput :: w ::: Maybe String <?> "Model output file"
                    , logOutput :: w ::: Maybe String <?> "Log output file"
                    , logLevel :: w ::: Maybe Int <?> "Log level"
                    }
            | Apply { grammarInput :: w ::: Maybe String <?> "File of sentences to parse"
                    , sentence :: w ::: Maybe String <?> "Sentence to parse"
                    , sentenceInput :: w ::: Maybe String <?> "File of sentences to parse"                    
                    , parseOutput :: w ::: Maybe String <?> "Parse output file"
                    , logOutput :: w ::: Maybe String <?> "Log output file"
                    , logLevel :: w ::: Maybe Int <?> "Log level"
                    }
  deriving (Generic)                             
                                                            
instance ParseRecord (Args Wrapped)
deriving instance Show (Args Unwrapped)


main :: IO ()
main = do
  ps <- unwrapRecord "" :: IO (Args Unwrapped)

  let lvl = fromMaybe 0 (logLevel ps)
      lout = logOutput ps
      logger = defaultLogger & level .~ lvl
      
  runEff . runLog logger $ case ps of Train {..} -> do
                                        cfg <- liftIO $ case trainingInput of Nothing -> return exampleGrammar
                                                                              Just fname -> do
                                                                                prds <- Text.readFile fname
                                                                                trainingData <- TreeBank.load prds
                                                                                return $ (CFG.fromTreebank trainingData :: CFG String String Int)
                                        log 1 "Initial grammar:"
                                        log 1 $ show cfg
  
                                        let cfg' = CFG.toCNF "START" "NEW" "'" cfg
                                            pcfg = CFG.toPCFG cfg'
                                            ofd = liftM2 openFile grammarOutput (Just WriteMode) :: Maybe (IO Handle)
                                        case ofd of Nothing -> return ()
                                                    Just ofd' -> liftIO $ ofd' >>= \d -> hPrint d pcfg >> hClose d

                                        log 3 $ "CNF probabilistic grammar:"
                                        log 3 $ show pcfg
                                      Apply {..} -> do
                                        --(prob, tr) <- parse pcfg toks
                                        --log 1 $ "Drawing likeliest parse tree (with log-probability of " ++ ((show . getSum) prob) ++ "):"
                                        --log 1 $ drawTree $ fmap (\x -> case x of Left s -> take 10 s; Right s -> s) tr    
                                        return ()

    


