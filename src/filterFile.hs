import PDBParser
import System.Environment
import Control.Monad (liftM)

main :: IO ()
main = do filePath <- liftM head getArgs
          proteinFile <- readFile filePath
          let ats = reNumAtoms (reNumResidues (filter (\at -> head (name at) /= 'H') $ extractAtoms proteinFile) 1) 1
          let ats' = map (\at -> at{altLoc=' ', occupancy=1.0}) ats
          mapM_ print ats'
          putStrLn "END"
