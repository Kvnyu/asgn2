
module Player where
import Data.Maybe
import Prelude
import Parser.Instances
import Data.List
import Parser.Parser -- This is the source for the parser from the course notes
import Rummy.Types   -- Here you will find types used in the game of Rummy
import Cards         -- Finally, the generic card type(s)
-- You can add more imports if you need them

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
  show (Card s r) = "(" ++ show s ++ show r ++ ")"

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
                      } deriving (Show)
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
cardParser ::  Parser Card
cardParser = do  x <- suitParser
                 y <- rankParser
                 return (Card x  y)
-- | This function returns a parser that continues producing a list of values form the given parser
-- Credit to week11 workshop
list :: Parser a -> Parser [a]
list a = list1 a ||| pure []
-- | This function returns a parser that produces at least one value from the given parser then continues producing a list of values from the given parser
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

-- | This function turns a list of cards into the string memory
makeMem :: [Card] -> String
makeMem cards = foldl (\acc curr -> acc ++ show curr) "" cards

-- | This function gets the value out of a parser, credit to the parsercombinators notes
getMem :: ParseResult a -> a
getMem (Result _ cs) = cs
getMem (Error _) = error "Hopefully it never comes to this"

-- | This function combines the list parser and cardParser to parse the memory
memoryParser :: Parser [[Card]]
memoryParser = sepby (list cardParser) (is '/')

-- | This card is called at the beginning of your turn, you need to decide which
-- pile to draw from.
pickCard :: ActionFunc
pickCard card _ memory opp hand
  | completesMeld hand card = (Discard, "hey")
  | otherwise = (Stock, "hey")

-- | This function is called once you have drawn a card, you need to decide
-- which action to call.
playCard :: PlayFunc
playCard card _ memory hand
  | gin = (Action Gin dropCard, newMemory)
  | knock = (Action Knock dropCard, newMemory)
  | otherwise = (Action Drop dropCard, newMemory)
  where dropCard = dropHighCard (hand)
        newHand = removeElements (card:hand) [dropCard]
        gin = canGin newHand
        knock = canKnock newHand
        newMemory = "hey"

--  where card = highestCard.(removeElements hand )(createBestMeld hand)
-- | This function is called at the end of the game when you need to return the
-- melds you formed with your last hand.
makeMelds :: MeldFunc
makeMelds _ _ hand = assembleMeld (createBestMeld hand) hand
-- | This function chooses the highest deadwood card to drop
-- TODO: THIS CAN BREAK IF YOU HAVE A GIN WITH THE EXTRA CARD (I mean it might wrongly pick the highest card. In the case that the whole hand is a Gin, just drop the card that you just picked up or drop a card from a 4/5 meld)
-- Examples:
-- >>>let a = [Card Spade Ace, Card Spade Two, Card Spade Three, Card Spade Four, Card Spade Jack, Card Spade Queen, Card Diamond Eight, Card Spade King, Card Diamond Jack, Card Club Ten]
-- >>>dropHighCard a
-- (DJ)
dropHighCard :: [Card] -> Card
dropHighCard hand =  highestCard . removeElements hand $ melds
  where melds = if createBestMeld hand == [] then [] else head (createBestMeld hand)
-- | This function checks whether a card completes a meld in a hand
-- Examples:
-- >>>let a = [Card Spade Ace, Card Spade Two, Card Spade Three, Card Spade Four, Card Spade Jack, Card Spade Queen, Card Diamond Eight, Card Spade King, Card Diamond Ace, Card Club Ace]
-- >>>let b = Card Heart Ace
-- >>>completesMeld a b
-- True
-- >>>let c = Card Heart Jack
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
-- [(SA),(S2),(C8),(DJ),(HA)]
-- >>>sortRank[Card Spade Nine, Card Heart Eight, Card Spade Six, Card Heart Five, Card Spade Jack]
-- [(S6),(S9),(SJ),(H5),(H8)]
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
-- [(SA),(SQ),(SK),(C6),(CJ),(D10),(DJ)]
sortRank :: [Card] -> [Card]
sortRank cards = sort cards

-- | Sort a list of cards on suit and then rank
--
-- Examples:
--
-- >>>sortSuit [Card Spade Two, Card Heart Two, Card Spade Ace, Card Club Ace, Card Diamond Jack, Card Club Eight, Card Spade Jack, Card Diamond Ace]
-- [(SA),(CA),(DA),(S2),(H2),(C8),(SJ),(DJ)]
-- >>>sortSuit[Card Spade Nine, Card Heart Eight, Card Spade Six, Card Heart Five, Card Spade Jack]
-- [(H5),(S6),(H8),(S9),(SJ)]
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
-- (SK)
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
-- [[(SA),(S2),(S3)],[(S2),(S3),(S4)],[(S3),(S4),(S5)]]
-- >>>generateGroups [] 3
-- []
generateGroups :: [Card] -> Int -> [[Card]]
generateGroups cards n = if length cards >= n then take n cards : generateGroups (drop 1 cards) n else []

-- | Filters a list of subsequences to find valid runs
--
-- Examples:
-- >>>let a = generateGroups[Card Spade Ace, Card Spade Two, Card Spade Three, Card Spade Four, Card Spade Five] 3
-- >>>filterRuns a
-- [[(SA),(S2),(S3)],[(S2),(S3),(S4)],[(S3),(S4),(S5)]]
-- >>>let b = generateGroups[Card Spade Ace, Card Spade Three, Card Spade Four, Card Spade Five, Card Spade Seven] 3
-- >>>filterRuns b
-- [[(S3),(S4),(S5)]]
-- >>>filterRuns []
-- []

filterRuns :: [[Card]] -> [[Card]]
filterRuns cardL = filter isRun cardL

-- | Filters a list of subsequences to find valid sets
--
-- Examples:
-- >>>let a = generateGroups[Card Spade Ace , Card Heart Ace, Card Club Ace, Card Diamond Ace, Card Heart Two, Card Club Two, Card Diamond Two , Card Club Three] 3
-- >>>filterSets a
-- [[(SA),(HA),(CA)],[(HA),(CA),(DA)],[(H2),(C2),(D2)]]
--
-- >>>let b = generateGroups[Card Spade Ace , Card Heart Ace, Card Club Ace, Card Diamond Ace, Card Heart Two, Card Club Two, Card Diamond Two , Card Club Three] 4
-- >>>filterSets b
-- [[(SA),(HA),(CA),(DA)]]


filterSets :: [[Card]] -> [[Card]]
filterSets cardL = filter isSet cardL


-- | Generates combinations of cards of size 3 from 4 cards
-- Examples:
-- >>>let a = [[Card Spade Ace, Card Heart Ace, Card Club Ace, Card Diamond Ace]]
-- >>>generateCombinations a
-- [[(SA),(HA),(CA)],[(SA),(HA),(DA)],[(SA),(CA),(DA)],[(HA),(CA),(DA)]]
-- >>>let b = [[Card Spade Ace, Card Heart Ace, Card Club Ace, Card Diamond Ace], [Card Club Ace, Card Diamond Ace, Card Heart Ace]]
-- >>>generateCombinations b
-- [[(CA),(DA),(HA)],[(SA),(HA),(CA)],[(SA),(HA),(DA)],[(SA),(CA),(DA)],[(HA),(CA),(DA)]]

generateCombinations :: [[Card]] -> [[Card]]
generateCombinations cardL = nub (foldl (\acc curr -> if setOfLengthFour curr then combinations curr 3 ++ acc else curr:acc) [] cardL)
   where setOfLengthFour cards = length cards == 4 && isSet cards

-- | Generates subsequences of size n from a list k
-- Examples:
-- >>>let a = [Card Spade Ace, Card Heart Ace, Card Club Ace, Card Diamond Ace]
-- >>>combinations a 3
-- [[(SA),(HA),(CA)],[(SA),(HA),(DA)],[(SA),(CA),(DA)],[(HA),(CA),(DA)]]
combinations :: [Card] -> Int -> [[Card]]
combinations cards n = filter ((n==).length) $ subsequences cards

-- | Builds a list of melds from a list of cards
--
-- Examples:
-- >>>let a = [Card Spade Ace, Card Heart Ace, Card Club Ace, Card Diamond Ace,Card Club Two, Card Club Three, Card Diamond Four, Card Club Five, Card Club Six]
-- >>>buildMelds a
-- [[(SA),(CA),(DA)],[(SA),(CA),(HA)],[(SA),(DA),(HA)],[(CA),(DA),(HA)],[(CA),(C2),(C3)]]

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
-- [(DA),(C2)]
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
            | showRank c `elem` ["10","J","Q","K"] = 10
            | otherwise =  read $ showRank c



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
-- Node ([],[(SA),(HA),(CA),(DA),(C2),(C3),(D4),(C5),(C6)]) [Node ([(SA),(CA),(DA)],[(HA),(C2),(C3),(D4),(C5),(C6)]) [],Node ([(SA),(CA),(HA)],[(DA),(C2),(C3),(D4),(C5),(C6)]) [],Node ([(SA),(DA),(HA)],[(CA),(C2),(C3),(D4),(C5),(C6)]) [Node ([(CA),(C2),(C3)],[(D4),(C5),(C6)]) []],Node ([(CA),(DA),(HA)],[(SA),(C2),(C3),(D4),(C5),(C6)]) [],Node ([(CA),(C2),(C3)],[(SA),(HA),(DA),(D4),(C5),(C6)]) [Node ([(SA),(DA),(HA)],[(D4),(C5),(C6)]) []]]

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
--[[[],[(SA),(CA),(DA)]],[[],[(SA),(CA),(HA)]],[[],[(SA),(DA),(HA)],[(CA),(C2),(C3)]],[[],[(CA),(DA),(HA)]],[[],[(CA),(C2),(C3)],[(SA),(DA),(HA)]]]

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
-- [[[(SA),(CA),(DA)]],[[(SA),(CA),(HA)]],[[(SA),(DA),(HA)],[(CA),(C2),(C3)]],[[(CA),(DA),(HA)]],[[(CA),(C2),(C3)],[(SA),(DA),(HA)]]]

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
-- [[[(SA),(DA),(HA)],[(CA),(C2),(C3)]],[[(CA),(C2),(C3)],[(SA),(DA),(HA)]]]

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
--[R3(SA)(S2)(S3),R3(S4)(S5)(S6),S4(S7)(S8)(S9)(S10)]


assembleMeld :: [[Card]] -> [Card] -> [Meld]
assembleMeld melds hand = m ++ deadwood
  where deadwood = map fromJust $ filter maybeFilter $ map createMeld $ [[x] | x <- removeElements hand $ concat melds]
        m = map fromJust $ filter maybeFilter $ map createMeld melds

maybeFilter :: Maybe a -> Bool
maybeFilter (Nothing) = False
maybeFilter (Just _) = True
-- | Creates a meld from a list of cards
-- Examples:
-- >>>let a = [Card Spade Ace, Card Heart Ace, Card Club Ace]
-- >>>createMeld a
-- S3(SA)(HA)(CA)
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
--[[(SA),(S2),(S3)],[(S4),(S5),(S6)],[(S7),(S8),(S9),(S10)]]
createBestMeld :: [Card] -> [[Card]]
createBestMeld hand =  head (minMelds (removeEmptyList $ paths tree) hand (findMinDeadwood tree))
  where tree = buildTree (buildMelds hand) [] hand

