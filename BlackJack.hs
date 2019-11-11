module BlackJack where
import Cards
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
