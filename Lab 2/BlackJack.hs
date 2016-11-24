{-
size hand2
= size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))        
= 1 + size (Add (Card Jack Spades) Empty)
= 1 + 1 + size Empty
= 1 + 1 + 0
= 2
-}
module BlackJack where
import Cards
import RunGame
import System.Random
import Test.QuickCheck hiding (shuffle)


-- An empty hand
empty :: Hand 
empty = Empty

hand1 = 
	(
	(Add (Card Jack Hearts)
	(Add (Card (Numeric 8) Spades)
	(Add (Card (Numeric 3) Clubs) 
	Empty)
	)))

hand2 = fullDeck

hand3 = (Add (Card (Numeric 2) Hearts) Empty)

-- Returns the numerical value corresponding to a rank
valueRank :: Rank -> Integer
valueRank (Numeric r) 	= r
valueRank Ace 			= 11
valueRank _ 			= 10

-- Returns the value of the cards rank
valueCard :: Card -> Integer
valueCard (Card r s) = valueRank r

-- Counts the number of aces in a given hand
numberOfAces :: Hand -> Integer
numberOfAces Empty 		= 0
numberOfAces (Add c h) 
	| rank c == Ace 	= 1 + numberOfAces h
	| otherwise			= numberOfAces h

-- Adds the values of every card in a hand. 
value :: Hand -> Integer
value h | (value' h) > 21 	= (value' h) - (10 * (numberOfAces h))
		| otherwise 		= value' h

-- Help function for value
value' :: Hand -> Integer
value' Empty = 0  
value' (Add c h) = valueCard c + value' h

-- Checks if Hands value is greater than 21, if so returns True
gameOver :: Hand -> Bool
gameOver h  | value h > 21 	= True
			| otherwise 	= False

-- Returns the winner 
winner :: Hand -> Hand -> Player
winner h1 h2 
	| gameOver h1	  		= Bank
	| gameOver h2 			= Guest
	| value h1 > value h2 	= Guest
	| otherwise				= Bank

-- Adds 2 hands together
(<+) :: Hand -> Hand -> Hand
(<+) Empty h2 		= h2
(<+) (Add c h1) h2 	= (Add c (h1 <+ h2))

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = (size h1) + (size h2) == (size(h1 <+ h2))


-- Creates a full deck
fullDeck :: Hand
fullDeck = 	foldr (<+) Empty 
			[fullSuit s | s <- [Hearts, Spades, Diamonds, Clubs]]

-- Help function for fullDeck. Creates a hand full of all the cards
-- in a given suit.
fullSuit :: Suit -> Hand
fullSuit s = 	foldr (<+) Empty 
				([Add (Card (Numeric a) s) Empty | a <- [2..10]] 
				++ [ Add (Card r s) Empty | r <- [Jack, Queen, King, Ace]])

-- Draws the top card of a deck.
draw :: Hand -> Hand -> (Hand,Hand)
draw (Add c h1) h2 	= ((h1),(Add c h2))
draw Empty h2 		= error "draw: The deck is empty."

-- Plays for the bank in line with the rules.
playBank :: Hand -> Hand
playBank h = playBank' h Empty

-- Help funciton for playBank. Draws until value >= 16 or bust
playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand 
	| value bankHand >= 16 	= bankHand
	| otherwise 			= playBank' deck' bankHand'
  where (deck',bankHand')   = draw deck bankHand

-- Shuffles a hand
shuffle :: StdGen -> Hand -> Hand
shuffle g Empty   = Empty
shuffle g h       = Add c' (shuffle g' h')
	where 	
		(c', h')  = pickCardN h n
		(n, g')   = randomR (1, (size h)) g	


--Given a StdGen and a Hand, generates a number between 1 and the deck size,
--as well as a new StdGen
--randomN :: StdGen -> Hand -> (Integer, StdGen)
--randomN g h = randomR (1, (size h)) g


-- Draws the card on position n and returns the card and the deck 
-- as a Hand w/o the card.
pickCardN :: Hand -> Integer -> (Card, Hand)
pickCardN Empty n = error "Hand is empty"
pickCardN (Add c h) n 
	| n < 1 	= error "Can't pick negative card"
	| n > size (Add c h) = error "Number can't be larger than size of hand"
	| n == 1 	= (c, h)
	| n > 1 	= (c', (Add c h'))
		where (c', h') = pickCardN h (n-1)

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h 		= size h == size (shuffle g h )

implementation = Interface
  { iEmpty    = empty
  , iFullDeck = fullDeck
  , iValue    = value
  , iGameOver = gameOver
  , iWinner   = winner 
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffle
  }

main :: IO ()
main = runGame implementation