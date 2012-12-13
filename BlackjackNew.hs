import System.Random
import Data.List
import Text.Printf
import Control.Monad.State

-- a blackjack simulator to measure effectiveness of tactics

data Card = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine |
            Ten | Jack | Queen | King deriving (Show, Eq, Enum)
type Hand = [Card]
type Deck = [Card]

fullDeck :: Deck
fullDeck = [Ace .. King] ++ [Ace .. King] ++ [Ace .. King] ++ [Ace .. King]

shuffleCards :: Deck -> Deck -> IO Deck
shuffleCards shuffled [] = return shuffled
shuffleCards shuffled unshuffled = do
  randomCardIndex <- randomRIO (0, length unshuffled - 1)
  let randomCard = unshuffled !! randomCardIndex
      unshuffledBefore = take randomCardIndex unshuffled
      unshuffledAfter = drop (randomCardIndex + 1) unshuffled
  
  shuffleCards (randomCard:shuffled) (unshuffledBefore ++ unshuffledAfter)

shuffleDeck :: IO Deck
shuffleDeck = shuffleCards [] fullDeck

cardValues :: Card -> [Int]
cardValues Ace   = [1, 11]
cardValues Two   = [2]
cardValues Three = [3]
cardValues Four  = [4]
cardValues Five  = [5]
cardValues Six   = [6]
cardValues Seven = [7]
cardValues Eight = [8]
cardValues Nine  = [9]
cardValues _     = [10]


countValue :: Card -> Int
countValue Two   = 1
countValue Three = 1
countValue Four  = 1
countValue Five  = 1
countValue Six   = 1
countValue Seven = 0
countValue Eight = 0
countValue Nine  = 0
countValue _     = -1




-- state is defined as (isDealerPlaying, cardCount, moneyMade, dealerPts, playerPts)
type GameState = (Bool, Int, Int, Int, Int, Hand, Hand);

startState = (False, 0, 0, 0, 0, [], []);

hit :: Hand -> Int -> Card -> (Hand, Int)
hit h p x = (newH, newP) where
            newH = x:h
            newP = case x of
                      Ace -> p+1
                      Two -> p+2
                      Three -> p+3
                      Four -> p+4
                      Five -> p+5
                      Six -> p+6
                      Seven -> p+7
                      Eight -> p+8
                      Nine -> p+9
                      _ -> p+10

-- list of cards -> card count - current money -> final money made
playGame :: [Card] -> Int -> Int -> Int
playGame [] _ m = m -- TODO : check length here: make sure we can still play one round
playGame l cnt m = undefined



-- list of cards -> card count -> money made in this round
roundPlay :: [Card] -> Int -> Int
roundPlay l cnt = let
                    initCards = take 4 l
                    curCnt = cnt + sum (map countValue initCards)
                    thisBet = determinBet curCnt
                    --(newL, playerScore) = playerPlayGame (take 2 initCards) curCnt (drop 4 l) 
                    --(newL', dealerScore) = dealerPlayGame (drop 2 initCards) newL
                    (newL, dealerScore) = dealerPlayGame (drop 2 initCards) l
                  in 
                  dealerScore
                    
-- player hand -> card count ->remaining deck -> (newRemainingDeck, playerScore)
playerPlayGame :: [Card] -> Int -> [Card] -> ([Card], Int)
playerPlayGame h cnt d = undefined

-- dealer hand -> remaining deck -> (newRemainingDeck, dealerScore)
dealerPlayGame :: [Card] -> [Card] -> ([Card], Int)
dealerPlayGame h d = let 
                       isHandSoft = handIsSoft h
                       score = handScore h
                     in 
                     if score > 17 then (d, score)
                     else if ((score == 17) && (isHandSoft==False)) then (d, score)
                          else dealerPlayGame ((head d):h) (tail d)
                       

handScore :: Hand -> Int
handScore hand = if notBustTotals == [] then 22 else (last notBustTotals)
                    where notBustTotals = filter (<= 21) $ possibleHandTotals hand [0]



handIsSoft :: Hand -> Bool
handIsSoft hand = Ace `elem` hand



possibleHandTotals :: Hand -> [Int] -> [Int]
possibleHandTotals [] totals = sort $ nub totals
possibleHandTotals (card:cards) runningTotals =
  possibleHandTotals cards newTotals
  where newTotals = [total + value | total <- runningTotals, value <- cardValues card]




{-
playGame l cnt m= 

roundplay l cnt 
where roundplay :: [Card] -> Int -> Int 

playGame (remainder cards) cnt m 

-}

{-
playGame :: [Card] -> State GameState GameState
playGame [] = do
    (isDealerPlaying, cardCount, money, dPts, pPts, dHand, pHand) <- get
    return (isDealerPlaying, cardCount, money, dPts, pPts, dHand, pHand)
playGame (x:xs) = do
    (isDealerPlaying, cardCount, money, dPts, pPts, dHand, pHand) <- get
    put (isDealerPlaying, cardCount, money, dPts, pPts, dHand, pHand)
    playGame xs
-}     
{-
      Ace -> put (isDealerPlaying, cardCount-1, money, dPts, pPts,dHand, pHand)
      Two -> put (isDealerPlaying, cardCount-1, money, dPts, pPts, dHand, pHand)
      Three -> put (isDealerPlaying, cardCount-1, money, dPts, pPts, dHand, pHand)
      Four -> put (isDealerPlaying, cardCount-1, money, dPts, pPts, dHand, pHand)
      Five -> put (isDealerPlaying, cardCount-1, money, dPts, pPts, dHand, pHand)
      Six -> put (isDealerPlaying, cardCount-1, money, dPts, pPts, dHand, pHand)
      Seven -> put (isDealerPlaying, cardCount, money, dPts, pPts, dHand, pHand)
      Eight -> put (isDealerPlaying, cardCount, money, dPts, pPts, dHand, pHand)
      Nine -> put (isDealerPlaying, cardCount, money, dPts, pPts, dHand, pHand)
      _   -> put (isDealerPlaying, cardCount+1, money, dPts, pPts, dHand, pHand)
    else
      put (isDealerPlaying, cardCount, money, dPts, pPts, dHand, pHand)
-}
--    playGame xs




-- main = print $ evalState (playGame fullDeck) startState
