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

--Takes a line of atom definitions. Removes all the ones that have a hydrogen defined at the end
removeHydrogens :: [String] -> [String]
removeHydrogens xs = [x | x <- xs, last (words x) /= "H" ]

-- Go through list of strings, numbering with the int from state
reNumberAtoms :: [String] -> Int -> [String]
reNumberAtoms [] _= []
reNumberAtoms (x:xs) i = reNumAtom x i : reNumberAtoms xs (i+1)
  where
    reNumAtom :: String -> Int -> String
    reNumAtom str j = intercalate " " (["ATOM", show j] ++ (drop 2 $ words str))

a=["hi H", "ther 1310298      23509809 8   H    ", "hajwhdkawjhd     C", "wlkjh   H awdlkajwhdlkjh  O"]

main :: IO ()
main = return (reNumberAtoms (removeHydrogens a) 1) >>= \as ->
  print as
