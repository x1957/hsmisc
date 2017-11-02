module App.Usage where
import           System.Environment (getProgName)

usageF actions = getProgName >>= mapM_ putStrLn . helps where
  helps cmd = "Usage:" :
    [ "\t" ++ (unwords $ cmd : cmds ++ ["<file>"])
    | cmds <- map fst actions ]

usage actions = getProgName >>= mapM_ putStrLn . helps where
  helps cmd = "Usage:" :
    [ "\t" ++ (unwords $ cmd : cmds) | cmds <- map fst actions ]

appF actions args = case args of
  [] -> usage'
  _  -> let (file : args') = reverse args
               in  case lookup (reverse args') actions of
    Just a -> a file
    _      -> usage'
  where usage' = usageF actions

app actions args = case lookup args actions of
  Just a -> a
  _      -> usage actions
