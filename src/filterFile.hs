import PDBParser
import System.Environment
import Control.Monad (liftM)

main :: IO ()
main = do filePath <- liftM head getArgs
          proteinFile <- readFile filePath
          let ats = reNumAtoms (filter (\at -> element at /= "H" && resSeq at >= 1 ) $ extractAtoms proteinFile) 1
          mapM_ print ats
          putStrLn "END"
