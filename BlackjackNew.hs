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

playGame :: [Card] -> State GameState GameState
playGame [] = do
    (isDealerPlaying, cardCount, money, dPts, pPts, dHand, pHand) <- get
    return (isDealerPlaying, cardCount, money, dPts, pPts, dHand, pHand)
playGame (x:xs) = do
    (isDealerPlaying, cardCount, money, dPts, pPts, dHand, pHand) <- get
    put (isDealerPlaying, cardCount, money, dPts, pPts, dHand, pHand)
    playGame xs
     
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




main = print $ evalState (playGame fullDeck) startState
