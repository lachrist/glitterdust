
import qualified System.Environment
import qualified Expression
import qualified Parser
import qualified Interpreter

main :: IO ()
main = do [path] <- System.Environment.getArgs
          str <- readFile path
          putStrLn $ case Parser.apply Expression.parse str
                     of [(expr, "")] -> Interpreter.run expr False    
                        xs            -> "parse-error " ++ show xs

