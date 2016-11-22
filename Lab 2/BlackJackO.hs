--BlackJack.hs

module BlackJack where
import Cards
import RunGame
import System.Random

{-
size hand2
  = size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
  = 1 + size (Add (Card Jack Spades) Empty)
  = 1 + 1 + size Empty
  = 1 + 1 + 0
  = 2
-}

--returns an empty hand
empty :: Hand
empty = Empty

--determines wether the aces should count for 1 or 11 points
value :: Hand -> Integer
value hand | value' hand > 21 = (value' hand) - (10*(numberOfAces hand))
value hand | otherwise        = value' hand

--calculates the value of a hand
value' :: Hand -> Integer
value' Empty           = 0
value' (Add card hand) = (valueCard card) + (value' hand)

--returns the value of a given rank
valueRank :: Rank -> Integer
valueRank (Numeric i)    = i
valueRank Ace            = 11  
valueRank _              = 10

--returns the value of a given card
valueCard :: Card -> Integer
valueCard (Card rank suit) = valueRank rank

--returns the number of aces in a given hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add card hand) | rank card == Ace = 1 + numberOfAces hand
                             | otherwise        =     numberOfAces hand

--calculates wether a hand results in a bust or not
gameOver :: Hand -> Bool
gameOver hand | (value hand) > 21 = True
gameOver hand | otherwise         = False

--given two hands, determines if the guest or the bank wins
winner :: Hand -> Hand -> Player
winner hand1 hand2 | gameOver hand1                = Bank
winner hand1 hand2 | gameOver hand2                = Guest
winner hand1 hand2 | (value hand1) > (value hand2) = Guest
winner hand1 hand2 | otherwise                     = Bank

--given 2 hands, puts the first hand on top of the other
(<+) :: Hand -> Hand -> Hand
Empty      <+ h2 = h2
(Add c h1) <+ h2 = (Add c (h1 <+ h2))

--tests that the (<+) function adds hands on top of each other properly
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

--tests that the (<+) function returns a hand with the correct size
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = (size h1) + (size h2) == (size (h1 <+ h2))

-- returns a full deck of cards
fullDeck :: Hand
fullDeck = foldr (<+) Empty
		   ([fullSuit suit | suit <- [Hearts, Diamonds, Clubs, Spades]])

--returns every card of a suit.
fullSuit :: Suit -> Hand
fullSuit suit = foldr (<+) Empty 
				([(Add (Card (Numeric n) suit) Empty) | n <-[2..10]] 
			 ++ [(Add (Card r suit) Empty) | r <- [Jack, Queen, King, Ace]])

--draw the top card from the deck to a hand
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty h2 = error "draw: The deck in empty."
draw (Add c h1) h2 = (h1,(Add c h2))

--plays for the bank, according to its set rules.
playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

--gets first element of a tuple
first :: (a, b) -> a
first (x,y) = x

--gets second element of a tuple
second :: (a, b) -> b
second (x,y) = y

--draws cards for the bank
playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand | value bankHand >= 16  = bankHand
						| otherwise   = playBank' (first (deck',bankHand'))
												  (second (deck',bankHand')) 
						where (deck',bankHand') = draw deck bankHand

--shuffles a hand
shuffle :: StdGen -> Hand -> Hand
shuffle g Empty = Empty
shuffle g (Add c Empty) = (Add c Empty)
shuffle g hand = second (removeCardN hand (size hand)) (<+) (shuffle (mkStdGen (size (second (removeCardN hand (size hand))))) hand)	

removeCardN :: Hand -> Integer -> (Card, Hand)
removeCardN (Add c h) n | n == 1 = (c, h)
removeCardN (Add c h) n | n > 1 = (c', ((Add c Empty) <+ h'))
	where (c', h') = removeCardN h (n-1)


