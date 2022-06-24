import GHC.Float (rationalToDouble)
import Data.List
data Die = One  | Two  | Three
         | Four | Five | Six
         deriving (Eq, Show, Ord, Enum)

data EmptyDie = EmptyDie
                deriving (Eq,Show,Ord,Enum)

data Player = Attacker | Defender
              | Equal
              deriving (Eq, Show)

data WinCount = WCount Player Int
                deriving Eq
instance Show WinCount where
  show (WCount p n) = concat [show p," wins ",show n," times"]

instance Ord WinCount where
  compare (WCount _ x) (WCount _ y) = compare x y

detWinner :: Die -> Die -> Player
detWinner attacker defender
  | attacker > defender = Attacker
  | otherwise = Defender

defender :: [Die]
defender = [One .. Six]

attacker :: [Die]
attacker = [One .. Six]

packedCases :: [(Die,Die)]
packedCases = (,) <$> attacker <*> defender

cases  :: [Player]
cases = detWinner <$> attacker <*> defender

labeledCases :: [(Player,(Die,Die))]
labeledCases = zip cases packedCases

countWinner :: [Player] -> (WinCount,WinCount,WinCount)
countWinner l = helper l (WCount Attacker 0, WCount Defender 0,WCount Equal 0)
  where helper :: [Player] -> (WinCount,WinCount,WinCount) -> (WinCount,WinCount,WinCount)
        helper [] winCount = winCount
        helper (x:xs) (WCount p1 n1, WCount p2 n2, WCount p3 n3)
          | x == p1 = helper xs (WCount p1 (n1+1), WCount p2 n2, WCount p3 n3)
          | x == p2 = helper xs (WCount p1 n1, WCount p2 (n2+2), WCount p3 n3)
          | otherwise = helper xs (WCount p1 n1, WCount p2 n2, WCount p3 (n3+3))


toDouble :: Integral a => a -> Double
toDouble n = rationalToDouble (toInteger n) 1


formatWinnerResults :: (WinCount,WinCount,WinCount) -> String
formatWinnerResults (x@(WCount p1 n1),y@(WCount p2 n2),z@(WCount p3 n3)) = 
  concat [show x," | ",show y," | ",show z,"\n",
          "Total scenarios: ", show (f1+f2+f3),"\n",
          show p1," probability ", show (f1 / (f1+f2+f3)),"\n",
          show p2," probability ", show (f2 / (f1+f2+f3)),"\n",
          show p3," probability ", show (f3 / (f1+f2+f3)),"\n"]
  where f1 = toDouble n1
        f2 = toDouble n2
        f3 = toDouble n3


---------------------------------------------------------------------------------
emptyDie :: [EmptyDie]
emptyDie = [EmptyDie]

emptyDieComplete :: [EmptyDie]
emptyDieComplete = replicate 6 EmptyDie
---------------------------------------------------------------------------------
---------------- oneDie1Complete ----------------
type OneDie = [Die]

oneDie :: OneDie
oneDie = [One .. Six]
---------------- oneDie2Complete ----------------
type OneDie2Complete = ([Die],[EmptyDie])

oneDie2CompleteCombinations :: [(Die,EmptyDie)]
oneDie2CompleteCombinations = (,) <$> oneDie <*> emptyDie

---------------- oneDie3Complete ----------------
type OneDie3Complete = ([Die], [EmptyDie], [EmptyDie])

oneDie3CompleteCombinations :: [(Die,EmptyDie,EmptyDie)]
oneDie3CompleteCombinations = (,,) <$> oneDie <*> emptyDie <*> emptyDie

---------------------------------------------------------------------------------
---------------- twoDice2Complete ----------------
type TwoDice2Complete = ([Die],[Die])

pairDice2Complete ::TwoDice2Complete
pairDice2Complete = (oneDie,oneDie)

pairDice2CompleteCombinations :: [(Die,Die)]
pairDice2CompleteCombinations = (,) <$> firstDie <*> secondDie
  where (firstDie,secondDie) = pairDice2Complete

---------------- twoDice3Complete ----------------
type TwoDice3Complete = ([Die],[Die],[EmptyDie])

pairDice3Complete :: TwoDice3Complete
pairDice3Complete = (oneDie,oneDie, emptyDieComplete)

pairDice3CompleteCombinations :: [(Die, Die, EmptyDie)]
pairDice3CompleteCombinations = (,,) <$> firstDie <*> secondDie <*> emptyDieComplete
  where (firstDie,secondDie,_) = pairDice3Complete

---------------------------------------------------------------------------------
---------------- threeDice3Complete ----------------
type ThreeDice = ([Die],[Die],[Die])

threeDice :: ThreeDice
threeDice = (oneDie,oneDie,oneDie)

threeDice3CompleteCombinations :: [(Die,Die,Die)]
threeDice3CompleteCombinations = (,,) <$> firstDie <*> secondDie <*> thirdDie
  where (firstDie,secondDie,thirdDie) = threeDice


---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
-- Types of Attack
-- 1 attacker vs 1 defender
oneVoneCombinations :: [(Die, Die)]
oneVoneCombinations = (,) <$> oneDie <*> oneDie

oneVoneResults :: [Player]
oneVoneResults = detWinner <$> oneDie <*> oneDie

oneVoneLabeledResults :: [(Player, (Die, Die))]
oneVoneLabeledResults = zip oneVoneResults oneVoneCombinations

oneVoneCountedResults :: (WinCount, WinCount,WinCount)
oneVoneCountedResults = countWinner oneVoneResults

oneVoneFormattedResults :: String
oneVoneFormattedResults = formatWinnerResults oneVoneCountedResults

------------------------------------------------
-- 2 attacker vs 1 defender
det2v1Winner :: (Die,Die) -> (Die,EmptyDie) -> Player
det2v1Winner (a1,a2) (d,_) = detWinner aHighest d 
  where aHighest = max a1 a2

twoVoneCombinations :: [((Die, Die), (Die, EmptyDie))]
twoVoneCombinations = (,) <$> pairDice2CompleteCombinations <*> oneDie2CompleteCombinations

twoVoneResults :: [Player]
twoVoneResults = det2v1Winner <$> pairDice2CompleteCombinations <*> oneDie2CompleteCombinations

twoVoneLabeledResults :: [(Player, ((Die, Die), (Die, EmptyDie)))]
twoVoneLabeledResults = zip twoVoneResults twoVoneCombinations

twoVoneCountedResults :: (WinCount, WinCount,WinCount)
twoVoneCountedResults = countWinner twoVoneResults

twoVoneFormattedResults :: String
twoVoneFormattedResults = formatWinnerResults twoVoneCountedResults

------------------------------------------------
-- 2 attacker vs 2 defender
order2Dice :: Die -> Die -> (Die,Die)
order2Dice d1 d2 = (max d1 d2, min d1 d2)

det2v2Winner :: (Die,Die) -> (Die,Die) -> Player
det2v2Winner (a1,a2) (d1,d2)
  | winnerHighest == winnerLowest = winnerHighest
  | otherwise = Equal
  where (aHighest,aLowest) = order2Dice a1 a2 
        (dHighest,dLowest) = order2Dice d1 d2
        winnerHighest = detWinner aHighest dHighest
        winnerLowest = detWinner aLowest dLowest

twoVtwoCombinations :: [((Die,Die), (Die,Die))]
twoVtwoCombinations = (,) <$> pairDice2CompleteCombinations <*> pairDice2CompleteCombinations

twoVtwoResults :: [Player]
twoVtwoResults = det2v2Winner <$> pairDice2CompleteCombinations <*> pairDice2CompleteCombinations

twoVtwoCountedResults :: (WinCount,WinCount,WinCount)
twoVtwoCountedResults = countWinner twoVtwoResults

twoVtwoFormattedResults :: String
twoVtwoFormattedResults = formatWinnerResults twoVtwoCountedResults

------------------------------------------------
-- 3 attacker vs 2 defender
get2Highest :: Die -> Die -> Die -> (Die,Die)
get2Highest d1 d2 d3 = (x,head y)
  where (x:y) = take 2 (reverse $ sort [d1,d2,d3])

det3v2Winner :: (Die,Die,Die) -> (Die,Die,EmptyDie) -> Player
det3v2Winner (a1,a2,a3) (d1,d2,_) 
  | winnerHighest == winnerLowest = winnerHighest
  | otherwise = Equal
  where (aHighest,aLowest) = get2Highest a1 a2 a3
        (dHighest,dLowest) = get2Highest d1 d2 One
        winnerHighest = detWinner aHighest dHighest
        winnerLowest = detWinner aLowest dLowest

threeVtwoResults :: [Player]
threeVtwoResults = det3v2Winner <$> threeDice3CompleteCombinations <*> pairDice3CompleteCombinations

threeVtwoCountedResults :: (WinCount, WinCount, WinCount)
threeVtwoCountedResults = countWinner threeVtwoResults

threeVtwoFormattedResults :: String
threeVtwoFormattedResults = formatWinnerResults threeVtwoCountedResults


------------------------------------------------
-- 3 attacker vs 1 defender
det3v1Winner :: (Die,Die,Die) -> (Die,EmptyDie,EmptyDie) -> Player
det3v1Winner (a1,a2,a3) (d,_,_) = detWinner x d
  where (x:_) = reverse $ sort [a1,a2,a3] 


threeVoneCombinations :: [((Die, Die, Die), (Die, EmptyDie, EmptyDie))]
threeVoneCombinations = (,) <$> threeDice3CompleteCombinations <*> oneDie3CompleteCombinations

threeVoneResults :: [Player]
threeVoneResults = det3v1Winner <$> threeDice3CompleteCombinations <*> oneDie3CompleteCombinations

threeVoneLabeledResults :: [(Player, ((Die, Die, Die), (Die, EmptyDie, EmptyDie)))]
threeVoneLabeledResults = zip threeVoneResults threeVoneCombinations

threeVoneCountedResults :: (WinCount, WinCount, WinCount)
threeVoneCountedResults = countWinner threeVoneResults

threeVoneFormattedResults :: String
threeVoneFormattedResults = formatWinnerResults threeVoneCountedResults

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
printFormattedResults :: String -> String -> IO ()
printFormattedResults label stats = do
  putStrLn ("------------------------------------------------\n"
            ++label++"\n"
            ++"------------------------------------------------\n"
            ++stats)


main :: IO ()
main = do
  printFormattedResults "One VS One Results" oneVoneFormattedResults
  printFormattedResults "Two VS One Results" twoVoneFormattedResults
  printFormattedResults "Two VS Two Results" twoVtwoFormattedResults
  printFormattedResults "Three VS Two Results" threeVtwoFormattedResults
  printFormattedResults "Three VS One Resluts" threeVoneFormattedResults