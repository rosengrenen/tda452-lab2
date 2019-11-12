module BlackJack where
import Cards
import System.Random
import RunGame
import Test.QuickCheck

-- AO -------------------------------------------------------------------------

hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)

sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                   (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty)
            , 1 + 1 + size Empty
            , 2]

-- A1 -------------------------------------------------------------------------

displayRank :: Rank -> String
displayRank (Numeric n) = show n
displayRank Jack = "Jack"
displayRank Queen = "Queen"
displayRank King = "King"
displayRank Ace = "Ace"

displaySuit :: Suit -> String
displaySuit Hearts = "Hearts"
displaySuit Spades = "Spades"
displaySuit Diamonds = "Diamonds"
displaySuit Clubs = "Clubs"

displayCard :: Card -> String
displayCard (Card rank suit) = (displayRank rank) ++ " of " ++ (displaySuit suit)

display :: Hand -> String
display Empty = ""
display (Add card hand) = (displayCard card) ++ "\n" ++ (display hand)

-- A2 -------------------------------------------------------------------------

valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Jack = 10
valueRank Queen = 10
valueRank King = 10
valueRank Ace = 11

initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add card hand) = valueRank (rank card) + initialValue hand

numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add card hand) | rank card == Ace = 1 + numberOfAces hand
                             | otherwise        = 0 + numberOfAces hand

value :: Hand -> Integer
value hand | initialValue hand > 21 = initialValue hand - 10 * numberOfAces hand
           | otherwise              = initialValue hand

-- A3 -------------------------------------------------------------------------

gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- A4 -------------------------------------------------------------------------

winner :: Hand -> Hand -> Player
winner guest _ | gameOver guest == True = Bank
winner guest bank | value guest > value bank = Guest
                  | otherwise                = Bank

-- B1 -------------------------------------------------------------------------

h1 = Add (Card (Numeric 1) Hearts) (Add (Card (Numeric 2) Hearts) (Add (Card (Numeric 3) Hearts) Empty))
h2 = Add (Card (Numeric 4) Hearts) (Add (Card (Numeric 5) Hearts) (Add (Card (Numeric 6) Hearts) Empty))
h3 = Add (Card (Numeric 7) Hearts) (Add (Card (Numeric 8) Hearts) (Add (Card (Numeric 9) Hearts) Empty))

reverseHand :: Hand -> Hand
reverseHand hand = rev Empty hand
  where rev acc Empty           = acc
        rev acc (Add card hand) = rev (Add card acc) hand

(<+) :: Hand -> Hand -> Hand
(<+) Empty hand = hand
(<+) leftHand rightHand = (reverseHand leftHand) `concatenateHands` rightHand
  where concatenateHands Empty rightHand                   = rightHand
        concatenateHands (Add leftCard leftHand) rightHand = leftHand `concatenateHands` (Add leftCard rightHand)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf leftHand rightHand = size leftHand + size rightHand == size (leftHand <+ rightHand)

-- B2 -------------------------------------------------------------------------

ranks = [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]
suits = [Hearts, Spades, Diamonds, Clubs]
allCards = [Card rank suit | rank <- ranks, suit <- suits]

fullDeck :: Hand
fullDeck = cardsToHand Empty allCards
  where cardsToHand :: Hand -> [Card] -> Hand
        cardsToHand acc [] = acc
        cardsToHand acc (card:restOfCards) = cardsToHand (Add card acc) restOfCards

-- B3 -------------------------------------------------------------------------

--      Deck    Hand    (Deck, Hand)
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _ = error "draw: The deck is empty"
draw (Add deckCard deck) hand = (deck, Add deckCard hand)

-- B4 -------------------------------------------------------------------------

playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty

playBankHelper :: Hand -> Hand -> Hand
playBankHelper (Add deckCard deck) hand | value hand < 16 = playBankHelper deck (Add deckCard hand)
                                        | otherwise       = hand

-- B5 -------------------------------------------------------------------------

removeCard :: Hand -> Integer -> (Card, Hand)
removeCard Empty _                            = error "removeCard error: Empty hand"
removeCard (Add card deck) index | index == 0 = (card, deck)
removeCard (Add card deck) index              = (removedCard, (Add card restOfDeck))
  where (removedCard, restOfDeck) = removeCard deck (index - 1)

shuffleDeckAcc :: StdGen -> Hand -> Hand -> Hand
shuffleDeckAcc _ acc Empty = acc
shuffleDeckAcc g1 acc deck = shuffleDeckAcc g2 (Add card acc) newDeck
  where (randomVal, g2) = randomR (0, size deck - 1) g1
        (card, newDeck) = removeCard deck randomVal

shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck g deck = shuffleDeckAcc g Empty deck

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffleDeck g h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g hand = size hand == size (shuffleDeck g hand)
