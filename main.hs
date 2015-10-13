import Eval
import Parse
import System.Environment(getArgs)

main = do
  path:args <- getArgs
  sourse <- readFile path
  eval sourse
