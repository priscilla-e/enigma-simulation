{-- 
  COM2108: Functional Programming Assignment 2022

  Part 1: Simulation of the Enigma
  Part 2: Finding the Longest Menu 
  Part 3: Code Breaking – Simulating the Bombe

  Written by: Priscilla Emasoga
--}
module Enigma where
  import Debug.Trace
  import Data.Char
  import Data.Maybe
  import Data.List (maximumBy)
  import Data.Ord
  
{- Part 1: Simulation of the Enigma -}

  -- data types
  type Rotor = (String, Int) 
  type Reflector = [(Char, Char)]
  type Offsets = (Int, Int, Int) 
  type Stecker = [(Char, Char)]
  type Rotors = (Rotor, Rotor, Rotor)
  
  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
                | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Stecker
   

  {- encodeMessage: Top level function. This function takes an Enigma data type (SimpleEnigma or SteckeredEnigma) 
  and a message, returns the the encrypted version of message.

   Overview:
   - Message is cleaned before encryption i.e all non-alphabet characters are stripped out
   and all lowercase characters are converted into string.

   - An empty plugboard is passed to encodeStr for a SimpleEnigma
  -}
  encodeMessage :: String -> Enigma -> String
  encodeMessage msg (SimpleEnigma lr mr rr reflector offsets) 
      = encodeStr (clean msg) (lr, mr, rr) offsets reflector []
  encodeMessage msg (SteckeredEnigma lr mr rr reflector offsets stecker) 
      = encodeStr (clean msg) (lr, mr, rr) offsets reflector stecker

  {- steck: This function takes the Stecker and an alphabet, and return it's connected 
     pair in the baord.

   - if no steckered pair is found in the board, it returns the original alphabet.
  -}
  steck :: Stecker -> Char -> Char
  steck [] alpha = alpha
  steck ((c, c'):cs) alpha | c == alpha = c'
                           | c' == alpha = c
                           | otherwise = steck cs alpha

  {- innerShift: This function simulates the inner shift change of a letter before it is passed 
     to a rotor. 
     shifts a character DOWN e.g ('A, 1' => 'B')
  -}
  innerShift :: Char -> Int -> Char
  innerShift c n = chr ((alphaPos c + n) `mod` 26 + ord 'A' )

  {- outterShift: This function simulates the outer shift change of a letter after it has been encoded by
     a rotor.
     shifts a character UP e.g  ('K, 1' => 'J')
  -}
  outerShift :: Char -> Int -> Char
  outerShift c n = chr ((alphaPos c - n) `mod` 26 + ord 'A')

  {- encodeChar: Given a rotor and offset, this function encodes and 
     returns the encrypted letter.
  -}
  encodeChar :: Rotor -> Int -> Char -> Char
  encodeChar (str, _) offset c 
      = head ([outerShift x offset | (x, i) <- zip str [0..], i == alphaPos (innerShift c offset)])

  {- invertedEncodeChar: Given a rotor and offset, this function inversely encodes and
     returns the encrypted letter.
  -}             
  invertedEncodeChar :: Rotor -> Int -> Char -> Char
  invertedEncodeChar (str, _) offset c 
      = head ([outerShift (alphas !! i) offset | (x, i) <- zip str [0..], x == innerShift c offset])

  {- advance: Given a set of rotors and their offsets this function advances the offsets based on the 
     knock-on positions(kp) of each rotors. 
     returns the advance offsets
  -}      
  advance :: Rotors -> Offsets -> Offsets
  advance ((_,lrkp), (_,mrkp), (_,rrkp)) (ol, om, or) 
      | or' == rrkp && om' == mrkp = (ol', om', or')
      | or' == rrkp = (ol, om', or')
      | otherwise   = (ol, om, or')
         where 
            or' = (or + 1) `mod` 26
            om' = (om + 1) `mod` 26
            ol' = (ol + 1) `mod` 26
 
  {- passThrough: Given a set of rotors, offsets and a letter, this function encodes a letter 
     by passing it from the right to left rotor.
  -}
  passThrough :: Rotors -> Offsets -> Char -> Char
  passThrough (lr, mr, rr) (ol, om, or) 
      = encodeChar lr ol . encodeChar mr om . encodeChar rr or

  {- invertedPassThrough: Given a set of rotors, offsets and a letter, this function encodes a letter 
     in reverse by passing it from the left to right rotor.
  -}
  invertedPassThrough:: Rotors -> Offsets -> Char -> Char
  invertedPassThrough (lr, mr, rr) (ol, om, or) 
      = invertedEncodeChar rr or . invertedEncodeChar mr om . invertedEncodeChar lr ol

  {- reflect: This function takes the reflectorboard and an alphabet, and return it's connected 
     pair in the board.
     if no pair is found, it returns the original alphabet.
  -}
  reflect :: Reflector -> Char -> Char
  reflect [] alpha = alpha
  reflect ((c,c'):cs) alpha | c == alpha = c'
                            | c' == alpha = c
                            | otherwise = reflect cs alpha

  {- encodeStr: This function encodes each character in a string and 
     returns the encrypted string.
  -}
  encodeStr :: String -> Rotors -> Offsets -> Reflector -> Stecker -> String
  encodeStr [] _ _ _ _ = []
  encodeStr (c:cs) rotors offsets reflector stecker 
      = encodedChar : encodeStr cs rotors offsets' reflector stecker
         where
            offsets' = advance rotors offsets
            c' =  reflect reflector . passThrough rotors offsets' . steck stecker $ c
            encodedChar = steck stecker . invertedPassThrough rotors offsets' $ c'


{- Part 2: Finding the Longest Menu -}

  type Crib = [(Char, Char)]
  type Menu =  [Int] 
  type PosCrib = [(Int, Char, Char)] -- a crib with index positions attached

  {- longestMenu: Top level function,
      Given a crib, this function finds the longest menu (the indices of the longest chain) 
      in the crib.

      e.g [('A', 'B'), ('B','D'), ('Z','M')]  = [0,1]
  -}
  longestMenu :: Crib -> Menu
  longestMenu crib = longest $ genMenus crib' crib'
      where crib' = addPos crib 0

  {- findPaths: This function takes a pair in the crib, and generates a list of all the chain
   of pairs that can be reached from that position in the crib.
  -}
  findPaths :: (Int, Char, Char) -> PosCrib -> PosCrib -> [PosCrib]
  findPaths start crib visited = paths
      where 
         nexts = links start
         paths | null nexts = [[start]]
               | any (`elem` visited) nexts  = find $ filter (`notElem` visited) nexts
               | otherwise =  find nexts 
                  where 
                     find a = map (start :) $ concatMap (\next -> findPaths next crib (start:visited)) a
         links (_,_,y) = filter (\(_,x',_) -> y == x') crib
               
  {- genMenus: Given a crib, this function generates a list of all the menus in the crib -}
  genMenus :: PosCrib -> PosCrib -> [Menu]
  genMenus [] _ = []
  genMenus (x:xs) crib = menu : genMenus xs crib
      where menu = map (\(i,_,_) -> i) . longest $ findPaths x crib []

  {- addPos: Given a crib, this function adds index positions to each pair
     in the list.
  -}
  addPos :: Crib -> Int -> PosCrib
  addPos [] _ = []
  addPos ((c,c'):cs) n = (n,c,c') : addPos cs (n+1)


{- Part 3: Simulating the Bombe -}

  {- breakEnigma: Top level fucntion, Given a crib, this function generates its longest menu
  and finds a compatible set of offsets and Stecker. 
   -}
  breakEnigma :: Crib -> Maybe (Offsets, Stecker)
  breakEnigma crib = tryAllOffsets crib menu stecker (0,0,0)
      where 
         menu = longestMenu crib
         stecker = [(fst(crib !! head menu), 'A')]

  {- tryAllOffsets : This function recursively tries to find a set of offsets and Stecker using all possible 
  combinations of offsets starting from (0,0,0) to (25,25,25). 
  If it finds a result, it returns the Stecker and the pair of Offsets back to the breakEnigma function or 
  Nothing otherwise. 
  -} 
  tryAllOffsets :: Crib -> Menu -> Stecker -> Offsets -> Maybe (Offsets, Stecker)
  tryAllOffsets crib menu stecker offsets 
      | offsets' == (0,0,0) = Nothing
      | isNothing newStecker = tryAllOffsets crib menu stecker offsets'
      | otherwise = Just (offsets', fromMaybe' newStecker) 
         where 
            offsets' = advance' offsets
            newStecker = tryAllAlphas crib menu stecker offsets'
  
  {- tryAllAlphas: This function searches for a compatible Stecker for set of offsets.
  
   If a contradiction is found during the search, it makes the next assumption until all 
   characters in the alphabet are exhausted. In that case, it returns Nothing 
  -}
  tryAllAlphas :: Crib -> Menu -> Stecker -> Offsets -> Maybe Stecker
  tryAllAlphas crib menu [(x,y)] offsets 
      | y == 'Z' = Nothing
      | isNothing newStecker = tryAllAlphas crib menu nextStecker offsets
      | otherwise = newStecker
         where 
            newStecker = exploreMenu crib menu [(x,y)] offsets
            nextStecker = [(x, alphas !! (alphaPos y+1))] 

  {- exploreMenu: this function goes through the entire menu, building a steckboard until empty
   if contradiction, returns Nothing. 
  -}
  exploreMenu :: Crib -> Menu -> Stecker -> Offsets -> Maybe Stecker
  exploreMenu _ [] stecker _ = Just stecker
  exploreMenu crib (n:ns) stecker offsets  | isNothing newStecker = Nothing
                                           | otherwise = exploreMenu crib ns (fromMaybe' newStecker) offsets
      where 
         (plain, cipher) = crib !! n
         offsets' = iterate advance' offsets !! n
         plain' = steck stecker plain
         encoded = head (encodeMessage [plain'] (SimpleEnigma rotor1 rotor2 rotor3 reflectorB offsets'))
         newStecker = addStecker (encoded, cipher) stecker

  {- addStecker: adds a new pair to the stecker and returns the new stecker.
     returns Nothing if there is a contradiction i.e on of the chracter in the pair is already
     steckered to another character
   -}
  addStecker :: (Char, Char) -> Stecker -> Maybe Stecker
  addStecker (x, y) stecker   | x == y = Just stecker
                           | (x, y) `elem` stecker || (y, x) `elem` stecker = Just stecker
                           | contradiction = Nothing
                           | otherwise = Just ((x, y):stecker)
                              where contradiction = any (\(c, c') -> x == c || x == c' || y == c || y == c' ) stecker
  
  {- advance': Given a pair of offsets returns the next pair.
      Used exclusively in breakEnuga
    -}
  advance' :: Offsets -> Offsets
  advance' (ol, om, or)
      | or < 25   = (ol, om, or+1)
      | om < 25    = (ol, om+1, 0)
      | otherwise = (mod (ol + 1) 26, 0, 0)

{- Useful definitions and functions -}

   -- substitution cyphers for the Enigma rotors
   -- as pairs of (wirings, knock-on position)
   -- knock-on position is where it will cause the next left wheel to
   -- advance when it moves past this position
 
        --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  rotor1=("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)
  rotor2=("AJDKSIRUXBLHWTMCQGZNPYFVOE",5::Int)
  rotor3=("BDFHJLCPRTXVZNYEIWGAKMUSQO",22::Int)
  rotor4=("ESOVPZJAYQUIRHXLNFTGKDCMWB",10::Int)
  rotor5=("VZBRGITYUPSDNHLXAWMJQOFECK",0::Int)

  {- the standard Enigma reflector (Reflector B)
    swapped A<->Y, B<->R, C<->U,D<->H, E<->Q, F<->S, G<->L, 
            I<->P, J<->X, K<->N, M<->O, T<->Z,V<->W
  -}
  reflectorB= [('A','Y'),
              ('B','R'),
              ('C','U'),
              ('D','H'),
              ('E','Q'),
              ('F','S'),
              ('G','L'),
              ('I','P'),
              ('J','X'),
              ('K','N'),
              ('M','O'),
              ('T','Z'),
              ('V','W')]

  {- alphas: constant, returns a list off all alphabets
  -}
  alphas :: [Char]
  alphas = ['A'..'Z']

  {- alphaPos: given an uppercase letter, returns its index in the alphabet
     ('A' = position 0; 'Z' = position 25)
   -}
  alphaPos :: Char -> Int
  alphaPos c = ord c - ord 'A'

  {- clean: given a string, removes all non-alphabet characters and converts all lowercase characters
   to uppercase
  -}
  clean :: String -> String
  clean = filter isLetter . map toUpper

  {- longest: given a list of lists, returns the longest list in the list
  -}
  longest :: [[a]] -> [a]
  longest xss | null xss = []
              | otherwise = maximumBy (comparing length) xss
  
  {- fromMaybe': given a maybe type, return its value -}
  fromMaybe' :: Maybe a -> a
  fromMaybe' (Just x) = x