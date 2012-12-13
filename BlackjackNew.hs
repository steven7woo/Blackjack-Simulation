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

-- list of cards -> current money -> final money made
playGame :: [Card] -> Int -> Int
playGame l m = let
                 (l', moneyMade) = roundPlay l
                 m' = m + moneyMade
               in 
               if (length l)>10 then playGame l' m'
               else m'

-- card cnt -> bet
determineBet :: Int -> Int
determineBet cnt = case cnt of 
                             2 -> 25
                             3 -> 50
                             4 -> 75
                             5 -> 100
                             6 -> 125
                             7 -> 150
                             8 -> 175
                             9 -> 200
                             10 -> 225
                             otherwise -> 5

getOpenCard :: [Card] -> Card
getOpenCard (a:b:c:d) = c
getOpenCard _ = error "how come!"
                  

getCurCnt :: [Card] -> Int
getCurCnt d =  - sum (map countValue d)

-- playScore -> dealerScore -> Bet -> money made
findMoneyMade :: Int -> Int -> Int -> Int
findMoneyMade ps ds bet = if ps > 21 then -bet
                          else if ds > 21 then bet
                               else if (ps > ds) then bet
                                    else if (ps < ds) then -bet else 0



-- list of cards-> (remainingDeck, money made in this round)
roundPlay :: [Card] -> ([Card], Int)
roundPlay l = let
                    initCards = take 4 l
                    curCnt = getCurCnt (drop 4 l)
                    -- curCnt = cnt + sum (map countValue initCards)
                    thisBet = determineBet curCnt
                    dealerOpenCard = getOpenCard l
                    (newL, playerScore) = playerPlayGame (take 2 initCards) dealerOpenCard (drop 4 l) 
                    --(newL', dealerScore) = dealerPlayGame (drop 2 initCards) newL
                    (newL', dealerScore) = dealerPlayGame (drop 2 initCards) newL
                    money = findMoneyMade playerScore dealerScore thisBet
                  in
                  (newL', money) 
                    
-- player hand -> opencard -> remaining deck -> (newRemainingDeck, playerScore)
playerPlayGame :: [Card] -> Card -> [Card] -> ([Card], Int)
playerPlayGame _ _ [] = ([], 0)
playerPlayGame h oc d = let
                              score = handScore h
                              ocScore = handScore [oc]
                            in
                            if score > 16 then (d, score)
                            else if (score > 12) && (ocScore < 7) then (d, score)
                                 else playerPlayGame ((head d):h) oc (tail d)

-- dealer hand -> remaining deck -> (newRemainingDeck, dealerScore)
dealerPlayGame :: [Card] -> [Card] -> ([Card], Int)
dealerPlayGame _ [] = ([], 0)
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


--shuffleDeck :: [Card] -> [Card]
--shuffleDeck d = undefined

main = do
         sd <- shuffleDeck 
         let money = playGame sd 0 
         print money


playGames :: Int -> IO ()
playGames 0 = print "End game"
playGames n = do
                sd <- shuffleDeck
                let money = playGame sd 0
                print money
                playGames (n-1)
              

--main = do
--         seed <- newStdGen
--         let rs = randomlist 52 seed
--         print rs


--randomlist :: Int -> StdGen -> [Int]
--randomlist n = take n . unfoldr (Just . random)


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
