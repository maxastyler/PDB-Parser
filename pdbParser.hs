import Text.ParserCombinators.ReadP
import Control.Monad.State.Lazy
import Data.List (intercalate)
data Line = End | Atoml Atom | Remarkl Remark deriving (Eq)

instance Show Line where
  show End = "END"
  show (Atoml a) = show a
  show (Remarkl a) = show a

data Amino = ARG | HIS | LYS | ASP | GLU | SER | THR | ASN | GLN | CYS | SEC |
           GLY | PRO | ALA | VAL | ILE | LEU | MET | PHE | TYR | TRP deriving (Show, Eq, Read)

data Remark = Remark {
  remarkId :: Int,
  remarkStr :: String
                     } deriving (Eq)

instance Show Remark where
  show (Remark i s) = intercalate "\n" ["REMARK " ++ show i ++ " " ++ st | st <- lines s]

data Atom = Atom {
  atomId :: Int,
  elementStr :: String,
  amino :: Amino,
  peptide :: String,
  acidN :: Int,
  x :: Double,
  y :: Double,
  z :: Double,
  occupancy :: Double,
  iso :: Double,
  element :: String
                 } deriving (Eq)

instance Show Atom where
  show (Atom i est am pe acid ax ay az occ is ele) = intercalate " " ["ATOM", show i, est,
                                                                      show am, pe, show acid,
                                                                      show ax, show ay, show az,
                                                                      show occ, show is, ele]

atString = "ATOM 3 HI GLY HO 20 0.2 0.423 1 2.023 -0.23 H"

mergeRemarks :: Remark -> Remark -> Remark
mergeRemarks (Remark i s1) (Remark _ s2) = Remark i (s1++"\n"++s2)

pdblines = [Remarkl $ Remark 1 "hi", Remarkl $ Remark 1 "whoo", Remarkl $ Remark 1 "\nawdlkawjh\n",
           End, Remarkl $ Remark 3 "hi"]

main :: IO ()
main = do
  --print $ Atoml $ Atom 3 "HI" GLY "HI" 30 0.22 0.22 0.22 1.00 0.00 "H"
  print $ Remarkl $ Remark 200 "Hello there\nI AM A TEST REMARK\n IS THIS WORKING?"
