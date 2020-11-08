module Player where
import Data.Maybe
import Prelude
import Parser.Instances
import Data.List
import Parser.Parser -- This is the source for the parser from the course notes
import Rummy.Types   -- Here you will find types used in the game of Rummy
import Cards         -- Finally, the generic card type(s)
-- You can add more imports if you need them




updateHand :: Memory -> [Card] -> Memory
updateHand mem newHand= Memory {discard = discard mem, oppTake = oppTake mem,
                                oppNoWant = oppNoWant mem, lastHand = newHand}



-- | This function updates the memory using the data about the opponent drawing
-- Examples:
-- >>> let a = Just Discard
-- >>> let b = Memory [Card Spade Ace, Card Diamond Two]  [Card Spade Nine]  [Card Spade Jack] [Card Spade King]
-- >>> oppDrawUpdateMemory a b
--Memory {discard = [SA], oppTake = [S9,D2], oppNoWant = [SJ], lastHand = [SK]}
-- >>>let c = Just Stock
-- >>>oppDrawUpdateMemory c b
--Memory {discard = [SA,D2], oppTake = [S9], oppNoWant = [SJ,D2], lastHand = [SK]}
-- >>>let d = Memory [] [] [] []
-- >>>oppDrawUpdateMemory a d
-- Memory {discard = [], oppTake = [], oppNoWant = [], lastHand = []}
--
oppDrawUpdateMemory :: Maybe Draw -> Memory -> Memory
oppDrawUpdateMemory (Just Discard) (mem) = Memory {discard = if discard mem == [] then [] else init (discard mem)
                                                  ,oppTake = (oppTake mem) ++ discardTopCard mem
                                                  ,oppNoWant = oppNoWant mem
                                                  ,lastHand = lastHand mem }
oppDrawUpdateMemory (Just Stock) (mem) = Memory {discard = discard mem
                                                , oppTake = oppTake mem
                                                , oppNoWant = (oppNoWant mem) ++ discardTopCard mem
                                                , lastHand = lastHand mem }
oppDrawUpdateMemory Nothing mem = mem


discardTopCard :: Memory -> [Card]
discardTopCard mem = if length (discard mem) == 0 then [] else [last (discard mem)]
-- | Adds the opponent's discard to the memory
-- >>> let b = Memory [Card Spade Ace, Card Diamond Two]  [Card Spade Nine]  [Card Spade Jack] [Card Spade King]
-- >>>addDiscard True (Card Spade King) b
-- Memory {discard = [SA,D2], oppTake = [S9], oppNoWant = [SJ,SK], lastHand = [SK]}
-- >>>addDiscard False (Card Spade King) b
-- Memory {discard = [SA,D2,SK], oppTake = [S9], oppNoWant = [SJ,SK], lastHand = [SK]}

addDiscard :: Bool -> Card -> Memory -> Memory
addDiscard takeDiscard card mem = Memory {discard = (discard mem) ++ (if takeDiscard then [] else [card])
                                          ,oppTake = oppTake mem
                                          ,oppNoWant = (oppNoWant mem) ++ [card]
                                          ,lastHand = (lastHand mem)}

pickCard :: ActionFunc
pickCard card _ memory opp hand
  | takeDiscard = (Discard, serMemory)
  | otherwise = (Stock, serMemory)
  where takeDiscard = completesMeld hand card
        desMemory = maybeStringToListMemory memory
        drawMemory = oppDrawUpdateMemory opp desMemory
        discardMemory = addDiscard takeDiscard card drawMemory
        serMemory = memoryToString discardMemory

instance Show Action where
  show (Action act card) = (show act) ++ (show card)
-- | This function is called once you have drawn a card, you need to decide
-- which action to call.
-- >>>let a = Card Spade Jack
-- >>>let b = (0,0)
-- >>>let c = "///"
-- >>>let e = "CACACACACA//"
-- >>>let d = [Card Spade Ace, Card Spade Two, Card Spade Three, Card Spade Four, Card Spade Five, Card Spade Six, Card Spade Seven, Card Spade Eight, Card Spade Nine, Card Spade Ten]
-- >>>playCard a b c d
--(DropST,"ST///SJSAS2S3S4S5S6S7S8S9")
playCard :: PlayFunc
playCard card _ memory hand
  | gin = (Action Gin dropCard, serMemory)
  | knock = (Action Knock dropCard, serMemory)
  | otherwise = (Action Drop dropCard, serMemory)
  where deckWeights = createWeightedDeck
        discardWeights = discardListAlterCardWeight (oppNoWant newMemory) deckWeights
        wantWeights = wantListAlterCardWeight (oppTake newMemory) discardWeights
        dropCard = dropHighCard (hand) wantWeights
        newHand = removeElements (card:hand) [dropCard]
--      Here I am checking if the current hand is equal to the last hand, to check if we are in the first turn of a new
--      round and therefore cannot gin/knock
        gin = (canGin newHand) && ((lastHand desMemory == hand))
        knock = (canKnock newHand) && ((lastHand desMemory == hand))
        desMemory = stringToListMemory memory
        newMemory = if lastHand desMemory /= hand then Memory [] [] [] [] else desMemory
        discardMemory = Memory ((discard newMemory) ++ [dropCard]) (oppTake newMemory) (oppNoWant newMemory) (lastHand newMemory)
        handMemory = updateHand discardMemory newHand
        serMemory = memoryToString handMemory


--  where card = highestCard.(removeElements hand )(createBestMeld hand)
-- | This function is called at the end of the game when you need to return the
-- melds you formed with your last hand.
makeMelds :: MeldFunc
makeMelds _ _ hand = assembleMeld (createBestMeld hand) hand


instance Show Suit where
  show Spade = "S"
  show Club = "C"
  show Diamond = "D"
  show Heart = "H"
instance Show Rank where
  show Ace = "A"
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"
  show Ten = "T"
  show Jack = "J"
  show Queen = "Q"
  show King = "K"

instance Show Card where
  show (Card s r) = show s ++ show r

instance Show Meld where
  show (Deadwood c1) = "DW" ++ show c1
  show (Set3 c1 c2 c3) = "S3" ++ show c1 ++ show c2 ++ show c3
  show (Set4 c1 c2 c3 c4) = "S4" ++ show c1 ++ show c2 ++ show c3 ++ show c4
  show (Straight3 c1 c2 c3) = "R3" ++ show c1 ++ show c2 ++ show c3
  show (Straight4 c1 c2 c3 c4) = "R4" ++ show c1 ++ show c2 ++ show c3 ++ show c4
  show (Straight5 c1 c2 c3 c4 c5) = "R5" ++ show c1 ++ show c2 ++ show c3 ++ show c4 ++ show c5

data Memory = Memory { discard :: [Card]
                     , oppTake :: [Card]
                     , oppNoWant :: [Card]
                     , lastHand :: [Card]
                      } deriving (Show)

data Weight = Weight {card :: Card, weight :: Int}
instance Show Weight where
  show (Weight card int) = show card ++ "-w" ++ show int
-- | Creates a deck of cards with weight attached to each card
-- >>>createWeightedDeck
--[SA-w0,S2-w0,S3-w0,S4-w0,S5-w0,S6-w0,S7-w0,S8-w0,S9-w0,ST-w0,SJ-w0,SQ-w0,SK-w0,CA-w0,C2-w0,C3-w0,C4-w0,C5-w0,C6-w0,C7-w0,C8-w0,C9-w0,CT-w0,CJ-w0,CQ-w0,CK-w0,DA-w0,D2-w0,D3-w0,D4-w0,D5-w0,D6-w0,D7-w0,D8-w0,D9-w0,DT-w0,DJ-w0,DQ-w0,DK-w0,HA-w0,H2-w0,H3-w0,H4-w0,H5-w0,H6-w0,H7-w0,H8-w0,H9-w0,HT-w0,HJ-w0,HQ-w0,HK-w0]


createWeightedDeck :: [Weight]
createWeightedDeck = map (\x -> Weight x 0) createDeck


-- | Finds the card with the lowest weight given a list of cards and a list of weights
-- This function cannot/should not accept empty weight or list of cards
-- Examples:
-- >>>let a = createWeightedDeck
-- >>>let b = [Card Spade Seven, Card Diamond Ace, Card Spade Ten, Card Spade Jack, Card Diamond Ten, Card Diamond Jack]
-- >>>let c = createWeightedDeck
-- >>>let b = [Card Spade Seven, Card Diamond Ace, Card Spade Ten, Card Spade Jack, Card Diamond Ten, Card Diamond Jack]
-- >>>let d = discardListAlterCardWeight b a
-- >>>e = [Card Spade Ace, Card Spade Two, Card Diamond King, Card Club Three]
-- >>>findLowestWeightCard e d
--SA-w-1

findLowestWeightCard :: [Card] -> [Weight] -> Weight
findLowestWeightCard cardl weights = foldl (\acc curr -> if weight curr < weight acc then curr else acc) (head filteredWeight) filteredWeight
  where filteredWeight = filter (\w -> elem (card w) cardl) weights



-- | Applies a function to the int weight of a weight datatype
-- >>>let a = Weight (Card Spade Ace) 2
-- >>>functionOnCardWeight (+2)  a
-- SA-w4

functionOnCardWeight:: (Int -> Int) -> Weight -> Weight
functionOnCardWeight f (Weight card int) =  Weight card $ f int


-- |This function alters the weights of cards that a related to cards in a list of cards
-- >>>let a = createWeightedDeck
-- >>>let b = [Card Spade Seven, Card Diamond Ace, Card Spade Ten, Card Spade Jack, Card Diamond Ten, Card Diamond Jack]
-- >>>discardListAlterCardWeight b a
--[SA-w-1,S2-w0,S3-w0,S4-w0,S5-w-1,S6-w-1,S7-w-1,S8-w-2,S9-w-3,ST-w-3,SJ-w-3,SQ-w-2,SK-w-1,CA-w-1,C2-w0,C3-w0,C4-w0,C5-w0,
-- C6-w0,C7-w-1,C8-w0,C9-w0,CT-w-2,CJ-w-2,CQ-w0,CK-w0,DA-w-1,D2-w-1,D3-w-1,D4-w0,D5-w0,D6-w0,D7-w-1,D8-w-1,D9-w-2,DT-w-3,DJ-w-3,
-- DQ-w-2,DK-w-1,HA-w-1,H2-w0,H3-w0,H4-w0,H5-w0,H6-w0,H7-w-1,H8-w0,H9-w0,HT-w-2,HJ-w-2,HQ-w0,HK-w0]
discardListAlterCardWeight :: [Card] -> [Weight] -> [Weight]
discardListAlterCardWeight cardl weightl = foldl (\acc curr -> discardAlterCardWeight acc curr) weightl cardl

-- | This function alters the weights of cards that are related to the cards in the list
wantListAlterCardWeight :: [Card] -> [Weight] -> [Weight]
wantListAlterCardWeight cardl weightl = foldl (\acc curr -> wantAlterCardWeight acc curr) weightl cardl

-- |This function alters the weights of cards that are related to a single card
-- It is like a parent function which is used to implement discardAlterCardWeight and wantAlterCardWeight
alterCardWeightFunction :: (Int -> Int) -> [Weight] -> Card -> [Weight]
alterCardWeightFunction f weights card = map (\(Weight c i) -> if c `elem` (relatedCards card) then functionOnCardWeight f (Weight c i) else Weight c i ) weights

-- | Lowers the probability that the opponent wants cards for all cards related to a particular card
--
-- Examples:
-- >>>let a = [Weight (Card Diamond Ten) 0, Weight (Card Spade Seven) 0, Weight (Card Spade Eight) 0, Weight (Card Spade Nine) 0, Weight (Card Spade Ten) 0, Weight (Card Spade Jack) 0, Weight (Card Diamond Eight) 0, Weight (Card Club King) 0 ]
-- >>>discardAlterCardWeight a $ Card Spade Ten
-- [DT-w-1,S7-w0,S8-w-1,S9-w-1,ST-w-1,SJ-w-1,D8-w0,CK-w0]
discardAlterCardWeight :: [Weight] -> Card -> [Weight]
discardAlterCardWeight = alterCardWeightFunction (\x -> x-1)

-- | Lowers the probability that the opponent wants cards for all cards related to a particular card
--
-- Examples:
-- >>>let a = [Weight (Card Diamond Ten) 0, Weight (Card Spade Seven) 0, Weight (Card Spade Eight) 0, Weight (Card Spade Nine) 0, Weight (Card Spade Ten) 0, Weight (Card Spade Jack) 0, Weight (Card Diamond Eight) 0, Weight (Card Club King) 0 ]
-- >>>wantAlterCardWeight a $ Card Spade Ten
-- [DT-w1,S7-w0,S8-w1,S9-w1,ST-w1,SJ-w1,D8-w0,CK-w0]
wantAlterCardWeight :: [Weight] -> Card -> [Weight]
wantAlterCardWeight = alterCardWeightFunction (\x -> x + 1)

-- | Returns a list of cards that are related to the card, this includes
-- Cards of the same value with a different suit, or cards that have the same suit but are close by (2 ranks away)
-- Examples:
-- >>>relatedCards $ Card Spade Seven
-- [C7,D7,H7,S5,S6,S7,S8,S9]
-- >>>relatedCards $ Card Spade Ace
--[CA,DA,HA,SA,S2,S3]

relatedCards :: Card -> [Card]
relatedCards (Card suit value) = set ++ run
  where set = [Card x value | x <- delete suit [Spade .. Heart]]
        run = [Card suit x | x <- [(lowerBound value 2) .. (upperBound value 2)]]

-- | Returns the rank that is n lower than the current rank and guards against the lowest rank Ace
-- Examples:
-- >>>lowerBound Ace 2
-- A
--
-- >]>>lowerBound Two 2
-- A
-- >>>lowerBound King 2
-- J
lowerBound :: Rank -> Int -> Rank
lowerBound rank 0 = rank
lowerBound rank int = if rank /= Ace then lowerBound (pred rank) (int - 1) else lowerBound rank (int -1)


-- | Returns the rank that is n higher than the current rank and guards against the highest rank King
-- Examples:
-- >>>upperBound King 2
-- K
-- >>>upperBound Eight 2
-- T
upperBound :: Rank -> Int -> Rank
upperBound rank 0 = rank
upperBound rank int = if rank /= King then upperBound (succ rank) (int -1) else upperBound rank (int -1 )


-- | Creates a deck of cards
-- >>>createDeck
-- [SA,S2,S3,S4,S5,S6,S7,S8,S9,ST,SJ,SQ,SK,CA,C2,C3,C4,C5,C6,C7,C8,C9,CT,CJ,CQ,CK,DA,D2,D3,D4,D5,D6,D7,D8,D9,DT,DJ,DQ,DK,HA,H2,H3,H4,H5,H6,H7,H8,H9,HT,HJ,HQ,HK]
createDeck :: [Card]
createDeck = [Card (suit) (value)| suit <- [Spade .. Heart], value <- [Ace .. King]]

-- | This converts a memory data type to a string
-- >>> a = Memory [Card Spade Ace] [Card Spade Ace] [Card Spade Ace, Card Diamond Two] [Card Spade King]
-- >>>memoryToString a
-- "SA/SA/SAD2/SK"
memoryToString :: Memory -> String
memoryToString (Memory a b c d) = makeMem(a) ++ "/" ++ makeMem(b) ++ "/" ++ makeMem(c) ++ "/" ++ makeMem(d)

-- | This converts the memory string within the parser to into a list of cards
-- Examples:
-- >>> a = Just "SA/SA/SAD2/SK"
-- >>>maybeStringToListMemory a
-- Memory {discard = [SA], oppTake = [SA], oppNoWant = [SA,D2], lastHand = [SK]}
-- >>>maybeStringToListMemory Nothing
-- Memory {discard = [], oppTake = [], oppNoWant = [], lastHand = []}
maybeStringToListMemory :: Maybe Input -> Memory
maybeStringToListMemory (Just string) = stringToListMemory string
maybeStringToListMemory Nothing = listMemoryToMemory [[],[],[],[]]

-- | This converts the memory string into a list of cards
-- Examples:
-- >>> a = "SA/SA/SAD2/SK"
-- >>>stringToListMemory a
-- Memory {discard = [SA], oppTake = [SA], oppNoWant = [SA,D2], lastHand = [SK]}



stringToListMemory :: Input -> Memory
stringToListMemory string = listMemoryToMemory $ getMem (parse (sepby (list cardParser) (is '/')) string)


listMemoryToMemory :: [[Card]] -> Memory
listMemoryToMemory lmem = case lmem of
                        (c1:c2:c3:c4:[]) -> Memory c1 c2 c3 c4
                        _ -> Memory [] [] [] []
-- | This function turns a list of cards into the string memory
-- Example:
-- >>> makeMem [Card Spade Ace, Card Spade Two, Card Diamond Three]
--"SAS2D3"
makeMem :: [Card] -> String
makeMem cards = foldl (\acc curr -> acc ++ show curr) "" cards

-- | This function gets the value out of a parser, credit to the parsercombinators notes

getMem :: ParseResult a -> a
getMem (Result _ cs) = cs
getMem (Error _) = error "Hopefully it never comes to this"

-- | Hardcoded Char -> Suit
suitParser :: Parser Suit
suitParser = (is 'S' >> pure Spade) |||
             (is 'C' >> pure Club) |||
             (is 'D' >> pure Diamond) |||
             (is 'H' >> pure Heart)
-- | Hardcoded Char -> Rank
rankParser :: Parser Rank
rankParser = (is 'A' >> pure Ace) |||
             (is '2' >> pure Two) |||
             (is '3' >> pure Three) |||
             (is '4' >> pure Four) |||
             (is '5' >> pure Five) |||
             (is '6' >> pure Six) |||
             (is '7' >> pure Seven) |||
             (is '8' >> pure Eight) |||
             (is '9' >> pure Nine) |||
             (is 'T' >> pure Ten) |||
             (is 'J' >> pure Jack) |||
             (is 'Q' >> pure Queen) |||
             (is 'K' >> pure King)
-- | This function parses a card by combining the suit parser and rank parser
-- Examples:
-- >>>parse cardParser "SA"
-- Result >< SA
cardParser ::  Parser Card
cardParser = do  x <- suitParser
                 y <- rankParser
                 return (Card x  y)
-- | This function returns a parser that continues producing a list of values form the given parser
-- Credit to week11 workshop
list :: Parser a -> Parser [a]
list a = list1 a ||| pure []
-- | This function returns a parser that produces at least one value from the given parser
--  then continues producing a list of values from the given parser
-- Credit to week11 workshop
list1 :: Parser a -> Parser [a]
list1 p = do
         a <- p
         b <- list p
         c <- pure (a:b)
         return c
-- parse (sepby (list cardParser) (is '/')) "SASA/DK"
-- | This function produces a list of values coming off the given parser separated by the second given parser
sepby :: Parser a -> Parser s -> Parser [a]
sepby p s = (sepby1 p s) ||| pure []
-- | This function produces a non-empty list of values coming off a given parser separated bty the second parser
-- Credit to week11 workshop

sepby1 :: Parser a -> Parser s -> Parser [a]
sepby1 p s =
    do {v <- p
        ;w <- list (s *> p)
        ;pure (v:w)}



-- | This function chooses the highest deadwood card to drop
-- Examples:
-- >>>let a = [Card Spade Ace, Card Spade Two, Card Spade Three, Card Spade Four, Card Spade Jack, Card Spade Queen, Card Diamond Eight, Card Spade King, Card Diamond Jack, Card Club Ten]
-- >>>dropHighCard a
-- DJ
-- >>>let b = [Card Spade Ace, Card Spade Two, Card Spade Three, Card Spade Four, Card Spade Five, Card Spade Six, Card Spade Seven, Card Spade Eight, Card Spade Nine, Card Spade Ten]
-- >>>let melds = createBestMeld b
-- >>>dropHighCard b
-- ST

dropHighCard :: [Card] -> [Weight] -> Card
dropHighCard hand weights = card $ findLowestWeightCard (if deadWood == [] then (if pairs == [] then bigDiscard melds else pairs) else deadWood ) weights
  where melds = if createBestMeld hand == [] then [] else (createBestMeld hand)
        pairs = pairCards (removeElements hand $ concat melds)
        bigDiscard meld = last $ filter (\x -> length x >=4 ) meld
        deadWood = removeElements hand (pairs ++ (concat melds))
-- | This function checks whether a card completes a meld in a hand
-- Examples:
-- >>>let a = [Card Spade Ace, Card Spade Two, Card Spade Three, Card Spade Four, Card Spade Jack, Card Spade Queen, Card Diamond Eight, Card Spade King, Card Diamond Ace, Card Club Ace]
-- >>>let b = Card Heart Ace
-- >>>completesMeld a b
-- True-- >>>let c = Card Heart Jack
-- >>>completesMeld a c
-- False
-- >>>let d = Card Heart Ten
-- >>>completesMeld a d
-- False

completesMeld :: [Card] -> Card -> Bool
completesMeld hand card = findMinDeadwood (tree (card:hand)) <= (findMinDeadwood $ tree hand)
  where tree:: [Card] -> RoseTree
        tree h = buildTree (buildMelds h) [] h



-- | This function returns true if you can call gin and false otherwise
-- >>>let a = [Card Spade Ace, Card Spade Two, Card Spade Three, Card Spade Four, Card Spade Jack, Card Spade Queen, Card Diamond Eight, Card Spade King, Card Diamond Ace, Card Club Ace]
-- >>>canGin a
-- False
canGin :: [Card] -> Bool
canGin hand = findMinDeadwood tree == 0
  where tree = buildTree (buildMelds hand) [] hand


-- | This function returns true if you can call knock and false otherwise
-- >>>let a = [Card Spade Ace, Card Spade Two, Card Spade Three, Card Spade Four, Card Spade Jack, Card Spade Queen, Card Diamond Eight, Card Spade King, Card Diamond Ace, Card Club Ace]
-- >>>canKnock a
-- True
canKnock :: [Card] -> Bool
canKnock hand = findMinDeadwood tree < 10
  where tree = buildTree (buildMelds hand) [] hand

-- | Sort a list of cards on rank and then suit
--
-- Examples:
--
-- >>>sortRank[Card Spade Two, Card Spade Ace, Card Heart Ace, Card Diamond Jack, Card Club Eight]
-- [SA,S2,C8,DJ,HA]
-- >>>sortRank[Card Spade Nine, Card Heart Eight, Card Spade Six, Card Heart Five, Card Spade Jack]
-- [S6,S9,SJ,H5,H8]
-- >>>sortRank[]
-- []


-- | Gets the char rank out of a card
--
-- Examples:
-- >>>showRank $ Card Spade Two
-- "2"
-- >>>showRank $ Card Spade Jack
-- "J"
showRank :: Card -> String
showRank (Card _ r1) = show r1
-- | Sort a list of cards on rank and then suit
-- Examples:
-- >>>let a = [Card Spade Ace, Card Spade King, Card Spade Queen, Card Diamond Ten, Card Diamond Jack, Card Club Jack, Card Club Six]
-- >>>sortRank a
-- [SA,SQ,SK,C6,CJ,DT,DJ]
sortRank :: [Card] -> [Card]
sortRank cards = sort cards

-- | Sort a list of cards on suit and then rank
--
-- Examples:
--
-- >>>sortSuit [Card Spade Two, Card Heart Two, Card Spade Ace, Card Club Ace, Card Diamond Jack, Card Club Eight, Card Spade Jack, Card Diamond Ace]
-- [SA,CA,DA,S2,H2,C8,SJ,DJ]
-- >>>sortSuit[Card Spade Nine, Card Heart Eight, Card Spade Six, Card Heart Five, Card Spade Jack]
-- [H5,S6,H8,S9,SJ]
-- >>>sortSuit []
-- []

sortSuit :: [Card] -> [Card]
sortSuit cards = sortBy (\(Card s1 r1) (Card s2 r2) -> if r1 == r2 then compare s1 s2 else compare r1 r2) cards


-- | Check if two cards that have the same suit have an adjacent rank
--
-- Examples:
--
-- >>>adjacentCard (Card Spade Ace) (Card Spade Two)
-- True
-- >>>adjacentCard (Card Spade Ace) (Card Spade Three)
-- False
-- >>>adjacentCard (Card Spade Ace) (Card Heart Two)
-- False

adjacentCard :: Card -> Card -> Bool
adjacentCard c1 c2 = adjacentRank c1 c2 && equalSuit c1 c2

adjacentRank :: Card -> Card -> Bool
adjacentRank (Card _ r1) (Card _ r2) = if r1 /= King then succ r1 == r2 else False

equalRank :: Card -> Card -> Bool
equalRank (Card _ r1) (Card _ r2) = r1 == r2

equalSuit :: Card -> Card -> Bool
equalSuit (Card s1 _) (Card s2 _) = s1 == s2


-- | Finds the highest card out of a list of cards
--
-- Examples:
--
-- >>>let a = [Card Spade Ace, Card Spade King, Card Spade Queen, Card Diamond Ten, Card Diamond Jack, Card Club Jack, Card Club Six]
-- >>>highestCard a
--SK
highestCard :: [Card] -> Card
highestCard hand = last $ sortSuit hand

-- | Check if a list of cards is a run
--
-- Examples:
-- >>>isRun[ Card Spade Ace,Card Spade Two,Card Spade Three]
-- True
-- >>>isRun[Card Spade Ace, Card Spade Two, Card Heart Three]
-- False
-- >>>isRun[Card Spade Ace, Card Spade Three, Card Spade Four]
-- False

isRun :: [Card] -> Bool
isRun [] = False
isRun [_] = True
isRun (x:xs) = isRun xs && (adjacentCard x (head xs))

-- | Check if a list of cards is a set
--
-- Examples:
-- >>>isSet[Card Spade Ace, Card Diamond Ace, Card Club Ace]
-- True
-- >>>isSet[Card Spade Ace, Card Diamond Ace, Card Club Two]
-- False
-- >>>isSet[Card Spade Ace, Card Diamond Ace, Card Club Ace, Card Heart Ace]
-- True
-- >>>isSet[Card Spade Ace, Card Diamond Ace]
-- True
-- >>>isSet []
-- False
-- >>>isSet[Card Spade Ace]
-- True

isSet :: [Card] -> Bool
isSet [] = False
isSet [_] = True
isSet (x:xs) = equalRank x (head xs) && isSet xs
-- | Generates subsequences of size n from a list
--
-- Examples:
-- >>>generateGroups [Card Spade Ace, Card Spade Two, Card Spade Three, Card Spade Four, Card Spade Five] 3
-- [[SA,S2,S3],[S2,S3,S4],[S3,S4,S5]]
-- >>>generateGroups [] 3
-- []
generateGroups :: [Card] -> Int -> [[Card]]
generateGroups cards n = if length cards >= n then take n cards : generateGroups (drop 1 cards) n else []

-- | Filters a list of subsequences to find valid runs
--
-- Examples:
-- >>>let a = generateGroups[Card Spade Ace, Card Spade Two, Card Spade Three, Card Spade Four, Card Spade Five] 3
-- >>>filterRuns a
-- [[SA,S2,S3],[S2,S3,S4],[S3,S4,S5]]
-- >>>let b = generateGroups[Card Spade Ace, Card Spade Three, Card Spade Four, Card Spade Five, Card Spade Seven] 3
-- >>>filterRuns b
-- [[S3,S4,S5]]
-- >>>filterRuns []
-- []

filterRuns :: [[Card]] -> [[Card]]
filterRuns cardL = filter isRun cardL

-- | Filters a list of subsequences to find valid sets
--
-- Examples:
-- >>>let a = generateGroups[Card Spade Ace , Card Heart Ace, Card Club Ace, Card Diamond Ace, Card Heart Two, Card Club Two, Card Diamond Two , Card Club Three] 3
-- >>>filterSets a
-- [[SA,HA,CA],[HA,CA,DA],[H2,C2,D2]]
--
-- >>>let b = generateGroups[Card Spade Ace , Card Heart Ace, Card Club Ace, Card Diamond Ace, Card Heart Two, Card Club Two, Card Diamond Two , Card Club Three] 4
-- >>>filterSets b
-- [[SA,HA,CA,DA]]


filterSets :: [[Card]] -> [[Card]]
filterSets cardL = filter isSet cardL


-- | Generates combinations of cards of size 3 from 4 cards
-- Examples:
-- >>>let a = [[Card Spade Ace, Card Heart Ace, Card Club Ace, Card Diamond Ace]]
-- >>>generateCombinations a
-- [[SA,HA,CA,DA],[SA,HA,CA],[SA,HA,DA],[SA,CA,DA],[HA,CA,DA]]
-- >>>let b = [[Card Spade Ace, Card Heart Ace, Card Club Ace, Card Diamond Ace], [Card Club Ace, Card Diamond Ace, Card Heart Ace]]
-- >>>generateCombinations b
-- [[CA,DA,HA],[SA,HA,CA,DA],[SA,HA,CA],[SA,HA,DA],[SA,CA,DA],[HA,CA,DA]]

generateCombinations :: [[Card]] -> [[Card]]
generateCombinations cardL = nub (foldl (\acc curr -> if setOfLengthFour curr then [curr] ++ combinations curr 3 ++ acc else curr:acc) [] cardL)
   where setOfLengthFour cards = length cards == 4 && isSet cards

-- | Generates subsequences of size n from a list k
-- Examples:
-- >>>let a = [Card Spade Ace, Card Heart Ace, Card Club Ace, Card Diamond Ace]
-- >>>combinations a 3
-- [[SA,HA,CA],[SA,HA,DA],[SA,CA,DA],[HA,CA,DA]]
combinations :: [Card] -> Int -> [[Card]]
combinations cards n = filter ((n==).length) $ subsequences cards


-- | A modified version of the buildMelds function that instead builds pairs from a list of cards
-- Examples:
-- >>>let a = [Card Spade Ace, Card Diamond Two, Card Diamond Three, Card Spade Two, Card Diamond Ace]
-- >>>buildPairs a
-- [[SA,DA],[S2,D2],[SA,S2],[DA,D2],[D2,D3]]
-- >>>buildPairs []
-- []


buildPairs :: [Card] -> [[Card]]
buildPairs hand = nub (sets ++ runs)
  where sets = filterSets (foldl (\acc curr -> acc ++ generateGroups (sortSuit hand) curr) [] [2])
        runs = filterRuns (foldl (\acc curr -> acc ++ generateGroups (sortRank hand) curr) [] [2])

-- | A modified version of the buildMelds function that instead builds pairs from a list of cards
-- Examples:
-- >>>let a = [Card Spade Ace, Card Diamond Two, Card Diamond Three, Card Spade Two, Card Diamond Ace]
-- >>>pairCards a
-- [SA,DA,S2,D2,D3]
-- >>>pairCards []
-- []
pairCards :: [Card] -> [Card]
pairCards hand = nub $ concat $ buildPairs hand

-- | Builds a list of melds from a list of cards
--
-- Examples:
-- >>>let a = [Card Spade Ace, Card Heart Ace, Card Club Ace, Card Diamond Ace,Card Club Two, Card Club Three, Card Diamond Four, Card Club Five, Card Club Six]
-- >>>buildMelds a
-- [[SA,CA,DA,HA],[SA,CA,DA],[SA,CA,HA],[SA,DA,HA],[CA,DA,HA],[CA,C2,C3]]

buildMelds :: [Card] -> [[Card]]
buildMelds hand = nub (sets ++ runs)
  where sets = generateCombinations(filterSets ( foldl (\acc curr -> acc ++ generateGroups (sortSuit hand) curr) [] [3,4]))
        runs = filterRuns (foldl (\acc curr -> acc ++ generateGroups (sortRank hand) curr) [] [3,4,5])

-- | This funcion checks if a list is a subset of another list
-- Examples:
-- >>>let a = [Card Spade Ace, Card Heart Ace, Card Club Ace, Card Diamond Ace,Card Club Two, Card Club Three, Card Diamond Four, Card Club Five, Card Club Six]
-- >>>let b = [[Card Heart Ace], [Card Spade Ace], [Card Club Ace]]
-- >>>isSubsetOf (concat b) a
-- True

isSubsetOf :: [Card] -> [Card] -> Bool
isSubsetOf h1 h2 = all (flip elem h2) h1

-- | This function removes the elements in h2 from h1
-- Examples:
-- >>>let a = [Card Spade Ace, Card Heart Ace]
-- >>>let b = [Card Spade Ace, Card Heart Ace, Card Diamond Ace, Card Club Two]
-- >>>removeElements b a
-- [DA,C2]
removeElements :: [Card] -> [Card] -> [Card]
removeElements h1 h2 = filter (flip notElem h2) h1


-- | This function calculates the numerical value of a card
-- Examples:
-- >>>cardValue (Card Spade Ace)
-- 1
-- >>>cardValue (Card Spade King)
-- 10
-- >>>cardValue (Card Spade Eight)
-- 8
cardValue :: Card -> Int
cardValue c | showRank c == "A" = 1
            | showRank c `elem` ["T", "J", "Q", "K"] = 10
            | otherwise =  read (showRank c) :: Int



-- | This function calculates the deadwood value of a list of cards
-- Examples:
-- >>>let a = [Card Spade Ace, Card Heart Ace, Card Club Ace, Card Diamond Ace,Card Club Two, Card Club Three, Card Diamond Four, Card Club Five, Card Club Six]
-- >>>deadwoodCalculator a
-- 24
-- >>>deadwoodCalculator [Card Spade Two, Card Diamond Three, Card Spade King]
-- 15


deadwoodCalculator :: [Card] -> Int
deadwoodCalculator cards = sum $ map cardValue cards


data RoseTree = Node ([Card], [Card]) [RoseTree] deriving Show



-- | This function builds a rosetree of combinations of melds out of the list of melds
-- >>>let a = [Card Spade Ace, Card Heart Ace, Card Club Ace, Card Diamond Ace,Card Club Two, Card Club Three, Card Diamond Four, Card Club Five, Card Club Six]
-- >>>let b = buildMelds a
-- >>>buildTree b [] a
--Node ([],[SA,HA,CA,DA,C2,C3,D4,C5,C6]) [Node ([SA,CA,DA,HA],[C2,C3,D4,C5,C6]) [],Node ([SA,CA,DA],[HA,C2,C3,D4,C5,C6]) [],Node ([SA,CA,HA],[DA,C2,C3,D4,C5,C6]) [],Node ([SA,DA,HA],[CA,C2,C3,D4,C5,C6]) [Node ([CA,C2,C3],[D4,C5,C6]) []],Node ([CA,DA,HA],[SA,C2,C3,D4,C5,C6]) [],Node ([CA,C2,C3],[SA,HA,DA,D4,C5,C6]) [Node ([SA,DA,HA],[D4,C5,C6]) []]]



buildTree :: [[Card]] -> [Card] -> [Card] -> RoseTree
buildTree melds hand deadwood
  | children /= [] = Node (hand, deadwood) [buildTree (delete x melds) x (removeElements deadwood x) | x <- children]
  | otherwise = Node (hand, deadwood) []
  where children = filter (flip isSubsetOf deadwood) melds

-- |This function finds the min deadwood value from a rosetree of meld combinations
-- Examples:
-- >>>let a = [Card Spade Ace, Card Heart Ace, Card Club Ace, Card Diamond Ace,Card Club Two, Card Club Three, Card Diamond Four, Card Club Five, Card Club Six]
-- >>>let b = buildMelds a
-- >>>let c = buildTree b [] a
-- >>>findMinDeadwood c
-- 15
-- >>>let d = [Card Spade Ace, Card Spade Two, Card Spade Three, Card Spade Four, Card Spade Five, Card Spade Six, Card Spade Seven, Card Spade Eight, Card Spade Nine, Card Spade Ten]
-- >>>let e = buildMelds d
-- >>>let f = buildTree e [] d
-- >>>findMinDeadwood f
-- 0

findMinDeadwood :: RoseTree -> Int
findMinDeadwood (Node (_, deadwood) []) = deadwoodCalculator deadwood
findMinDeadwood (Node (_, _) children) = foldr min 200 $ map findMinDeadwood children


-- | Flattens a RoseTree's paths
-- Examples:
-- >>>let a = [Card Spade Ace, Card Heart Ace, Card Club Ace, Card Diamond Ace,Card Club Two, Card Club Three, Card Diamond Four, Card Club Five, Card Club Six]
-- >>>let b = buildMelds a
-- >>>let c = buildTree b [] a
-- >>>let d =  findMinDeadwood c
-- >>>paths c
--[[[],[SA,CA,DA,HA]],[[],[SA,CA,DA]],[[],[SA,CA,HA]],[[],[SA,DA,HA],[CA,C2,C3]],[[],[CA,DA,HA]],[[],[CA,C2,C3],[SA,DA,HA]]]

paths :: RoseTree -> [[[Card]]]
paths (Node (hand, _) []) = [[hand]]
paths (Node (hand,_) children) = map (hand:) $ concat $ map paths children
--paths (Node (hand,deadwood) children) = map ((:) hand . concat . paths) children

-- | Removes empty lists caused by empty children
-- Examples
-- >>>let a = [Card Spade Ace, Card Heart Ace, Card Club Ace, Card Diamond Ace,Card Club Two, Card Club Three, Card Diamond Four, Card Club Five, Card Club Six]
-- >>>let b = buildMelds a
-- >>>let c = buildTree b [] a
-- >>>let d =  findMinDeadwood c
-- >>>let e = paths c
-- >>>removeEmptyList e
-- [[[SA,CA,DA,HA]],[[SA,CA,DA]],[[SA,CA,HA]],[[SA,DA,HA],[CA,C2,C3]],[[CA,DA,HA]],[[CA,C2,C3],[SA,DA,HA]]]

removeEmptyList :: [[[Card]]] -> [[[Card]]]
removeEmptyList cardl = map (filter (/=[])) cardl

-- | Finds a path which has a deadwood count equal to the min deadwood
-- Examples:
-- >>>let a = [Card Spade Ace, Card Heart Ace, Card Club Ace, Card Diamond Ace,Card Club Two, Card Club Three, Card Diamond Four, Card Club Five, Card Club Six]
-- >>>let b = buildMelds a
-- >>>let c = buildTree b [] a
-- >>>let e = removeEmptyList $ paths c
-- >>>let min = findMinDeadwood c
-- >>>minMelds e a min
-- [[[SA,DA,HA],[CA,C2,C3]],[[CA,C2,C3],[SA,DA,HA]]]

minMelds :: [[[Card]]] -> [Card] -> Int -> [[[Card]]]
minMelds m hand smallest = (filter ((==smallest).deadwoodCalculator.(removeElements hand).concat) m)


-- | Assembles formal meld list given a list of melds and a hand
-- Examples:
-- >>>let a = [Card Spade Ace, Card Spade Two, Card Spade Three, Card Spade Four, Card Spade Five, Card Spade Six, Card Spade Seven, Card Spade Eight, Card Spade Nine, Card Spade Ten]
-- >>>let b = buildMelds a
-- >>>let c = buildTree b [] a
-- >>>let e = removeEmptyList $ paths c
-- >>>let min = findMinDeadwood c
-- >>>f = minMelds e a min
-- >>>assembleMeld (head f) a
--[R3SAS2S3,R3S4S5S6,R4S7S8S9ST]


assembleMeld :: [[Card]] -> [Card] -> [Meld]
assembleMeld melds hand = m ++ deadwood
  where deadwood = map fromJust $ filter maybeFilter $ map createMeld $ [[x] | x <- removeElements hand $ concat melds]
        m = map fromJust $ filter maybeFilter $ map createMeld melds

maybeFilter :: Maybe a -> Bool
maybeFilter (Nothing) = False
maybeFilter (Just _) = True
-- | Creates a meld from a list of cards
-- Examples:
-- >>>let b = [Card Spade Ace, Card Spade Two, Card Spade Three, Card Spade Four, Card Spade Five ]
-- >>>createMeld b
--Just R5SAS2S3S4S5
-- >>>let a = [Card Spade Ace, Card Heart Ace, Card Club Ace]
-- >>>createMeld a
--Just S3SAHACA
createMeld :: [Card] -> Maybe Meld
createMeld [c1] = Just $ Deadwood c1
createMeld three@(c1:c2:c3:[]) = if isSet three then Just $ Set3 c1 c2 c3 else Just $ Straight3 c1 c2 c3
createMeld four@(c1:c2:c3:c4:[]) = if isSet four then Just $ Set4 c1 c2 c3 c4 else Just $ Straight4 c1 c2 c3 c4
createMeld (c1:c2:c3:c4:c5:[]) = Just $ Straight5 c1 c2 c3 c4 c5
createMeld [] = Nothing
createMeld [_,_] = Nothing
createMeld (_:_:_:_:_:_:_) = Nothing
-- | Creates a list of melds given a hand
-- Examples:
-- >>>let a = [Card Spade Ace, Card Spade Two, Card Spade Three, Card Spade Four, Card Spade Five, Card Spade Six, Card Spade Seven, Card Spade Eight, Card Spade Nine, Card Spade Ten]
-- >>>createBestMeld a
--[[SA,S2,S3],[S4,S5,S6],[S7,S8,S9,ST]]
createBestMeld :: [Card] -> [[Card]]
createBestMeld hand =  head (minMelds (removeEmptyList $ paths tree) hand (findMinDeadwood tree))
  where tree = buildTree (buildMelds hand) [] hand

