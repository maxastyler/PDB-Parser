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

justifyLeft :: Int -> Char -> String -> String
justifyLeft i ch st
  | lst >= i = st
  | otherwise = st ++ (replicate (i-lst) ch)
    where lst = length st

justifyRight :: Int -> Char -> String -> String
justifyRight i ch st
  | lst >= i = st
  | otherwise = replicate (i-lst) ch ++ st
    where lst = length st

instance Show Remark where
  show (Remark i s) = intercalate "\n" ["REMARK " ++ show i ++ " " ++ st | st <- lines s]

-- Record strings are 80 characters in length. This is the format for an atom string
data Atom = Atom { -- Record Name 1 - 6 eg. "ATOM  "
  serial :: Int, -- 7 - 11
  name :: String, -- 13 - 16
  altLoc :: Char, -- 17
  resName :: Amino, -- 18 - 20
  chainID:: Char, -- 22
  resSeq :: Int, -- 23 - 26
  iCode :: Char, -- 27
  x :: Double, -- 31 - 38
  y :: Double, -- 39 - 46
  z :: Double, -- 47 - 54
  occupancy :: Double, -- 55 - 60
  tempFactor :: Double, -- 61 - 66 default of 0.0
  segment :: String, -- 73 - 76 left justified
  element :: String, -- 77 - 78 right justified
  charge :: String -- 79 - 80
                 } deriving (Eq)

instance Show Atom where
  show (Atom serialA nameA altLocA resNameA chainIDA resSeqA iCodeA xA yA zA
       occupancyA tempFactorA segmentA elementA chargeA) =
    let
      rJstTake :: Int -> String -> String
      rJstTake i str = justifyRight i ' ' $ take i str
      lJstTake :: Int -> String -> String
      lJstTake i str = justifyLeft i ' ' $ take i str
      serSt = rJstTake 5 $ show serialA
      namSt = rJstTake 4 $ nameA
      altSt = rJstTake 1 $ [altLocA]
      resSt = rJstTake 3 $ show resNameA
      chaSt = rJstTake 1 $ [chainIDA]
      rSqSt = rJstTake 4 $ show resSeqA
      iCoSt = rJstTake 1 $ [iCodeA]
      xSt = rJstTake 8 $ show xA
      ySt = rJstTake 8 $ show yA
      zSt = rJstTake 8 $ show zA
      occSt = rJstTake 6 $ show occupancyA
      temSt = rJstTake 6 $ show tempFactorA
      segSt = lJstTake 4 $ segmentA
      eleSt = rJstTake 2 $ elementA
      chrSt = rJstTake 2 $ chargeA
    in
      "ATOM  " ++ serSt ++ " " ++ namSt ++ altSt ++ resSt ++ " " ++ chaSt ++ rSqSt ++ iCoSt ++
      "   " ++ xSt ++ ySt ++ zSt ++ occSt ++ temSt ++ "      " ++ segSt ++ eleSt ++ chrSt

atomFromString :: String -> Atom
atomFromString str =
  let
    tkDrpStrp :: Int -> Int -> String -> String
    tkDrpStrp i j st = filter ((/=)' ') $ take i $ drop j st
    tkDrpStrpPos i j st = tkDrpStrp (j-i+1) (i-1) st
    serialA = read (tkDrpStrpPos 7 11 str) :: Int
    nameA = tkDrpStrpPos 13 16 str
    altLocA = case tkDrpStrpPos 17 17 str of
      c:cs -> c
      _ -> ' '
    resNameA = read $ tkDrpStrpPos 18 20 str :: Amino
    chainIDA = case tkDrpStrpPos 22 22 str of
      c:cs -> c
      _ -> ' '
    resSeqA = read $ tkDrpStrpPos 23 26 str :: Int
    iCodeA = case tkDrpStrpPos 27 27 str of
      c:cs -> c
      _ -> ' '
    xA = read $ tkDrpStrpPos 31 38 str :: Double
    yA = read $ tkDrpStrpPos 39 46 str :: Double
    zA = read $ tkDrpStrpPos 47 54 str :: Double
    occupancyA = read $ tkDrpStrpPos 55 60 str :: Double
    tempFactorA = read $ tkDrpStrpPos 61 66 str :: Double
    segmentA = tkDrpStrpPos 73 76 str
    elementA = tkDrpStrpPos 77 78 str
    chargeA = tkDrpStrpPos 79 80 str
  in
    Atom serialA nameA altLocA resNameA chainIDA resSeqA iCodeA
    xA yA zA occupancyA tempFactorA segmentA elementA chargeA


atString = "ATOM  32    HI  OGLY H 20 A   0.423   1 2.023 -0.23 H"
atoom = Atom 3 "AO020" 'p' GLY '2' 30 'a' 10.2030 30.2010 0.2313 0.23 0.0 "hol" "aoa" "hi"

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
reNumberAtoms (x:xs) i = if ((head $ words x) == "ATOM") then reNumAtom x i : reNumberAtoms xs (i+1)
  else
                           x : reNumberAtoms xs i
  where
    reNumAtom :: String -> Int -> String
    reNumAtom str j = intercalate " " (["ATOM", show j] ++ (drop 2 $ words str))

processPDBString :: String -> String
processPDBString str = intercalate "\n" $ reNumberAtoms (removeHydrogens (lines str)) 1

main :: IO ()
main = do proteinFile <- readFile "/home/max/mtyler88@gmail.com/University/4 Fourth Year/Senior Honours Project/Materials/protein/2wgo1.pdb"
          let fstAtom = take 500  $ lines proteinFile
          let at = map atomFromString fstAtom
          mapM_ print at
