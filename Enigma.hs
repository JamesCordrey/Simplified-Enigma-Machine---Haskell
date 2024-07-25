{-
  Author: James Cordrey
  Date: 08/12/2022

  This file contains all of the functions written for my solutions to the three sections of a grading assignment for my university course: 
  - Simulation of the Enigma, 
  - Finding the Longest Menu, 
  - Breaking the Enigma
-}

module Enigma where
  import Data.Char  -- to use functions on characters
  import Data.Maybe -- breakEnigma uses Maybe type
  
{- Part 1: Simulation of the Enigma -}

  type Rotor = (String, Int)
  type Reflector = [(Char, Char)]
  type Offsets = (Int, Int, Int)
  type Stecker = [(Char, Char)]
  
  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
                | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Stecker

  {- Takes a message to be encoded (String) and an Enigma which will be used to configure the machine for encoding.
     The String is converted to a list of Ints with the use of convertMessage before being passed into either 
     encodeSimple or encodeStecker depending on type of Enigma. -}
  encodeMessage :: String -> Enigma -> String
  encodeMessage m (SteckeredEnigma lr mr rr ref offs steck) = 
    convertBack (encodeStecker (convertMessage (cleanMessage m)) lr mr rr ref offs steck)
  encodeMessage m (SimpleEnigma lr mr rr ref offs) = 
    convertBack (encodeSimple (convertMessage (cleanMessage m)) lr mr rr ref offs)

  {- Takes a String and removes any character which is not an uppercase letter. 
     Uses the isIn function to check if each letter of the String is contained in cleanFilter -}
  cleanMessage :: String -> String
  cleanMessage x = convertUpper filtered
    where filtered = [n | n <-x, isIn n cleanFilter]

  {- A String used to filter out any none letter Chars -}
  cleanFilter = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

  {- Takes a String and converts any lowercase letters into uppercase letters -}
  convertUpper :: String -> String
  convertUpper [] = []
  convertUpper x
    | length x == 1 && alphaPos (head x) > 25 = [chr (alphaPos (head x) + 33)]
    | length x == 1 = x
  convertUpper (x:xs)
    | alphaPos x > 25 = chr (alphaPos x + 33) : convertUpper xs
    | otherwise = x : convertUpper xs

  {- Takes a String and converts each Char into its respective Int using alphaPos. -}
  convertMessage :: String -> [Int]
  convertMessage x = [alphaPos n | n <- x]

  {- Takes a list of Ints (indexes of alphabet) and converts each Int into its respective Char. -}
  convertBack :: [Int] -> String
  convertBack x = [chr (n + ord 'A') | n <- x, n >= 0 && n < 26]

  {- Takes a list of Ints (indexes of alphabet) and a Stecker and passes each Int from the input list through the 
     reflect function with the stecker as the reflector. -}
  applyStecker :: [Int] -> Stecker -> [Int]
  applyStecker [] _ = []
  applyStecker (x:xy) steck = reflect x steck : applyStecker xy steck

  {- Applies the stecker to the input list of Ints (indexes of alphabet) and then passes this into encodeSimple. 
     It then applies the stecker to the output of encodeSimple. -}
  encodeStecker :: [Int] -> Rotor -> Rotor -> Rotor -> Reflector -> Offsets -> Stecker -> [Int]
  encodeStecker x (lc,lknock) (mc,mknock) (rc,rknock) ref (lo, mo, ro) steck = 
    applyStecker (encodeSimple (applyStecker x steck)(lc,lknock) (mc,mknock) (rc,rknock) ref (lo, mo, ro)) steck

  {- Takes a list of Ints (indexes of alphabet), and the Enigma information and outputs the encoded String. 
     Calls encodeForward, reflect, encodeBackward, convertBack in that order. -}
  encodeSimple :: [Int] -> Rotor -> Rotor -> Rotor -> Reflector -> Offsets -> [Int]
  encodeSimple [] _ _ _ _ _ = []
  encodeSimple (x:xs) lr mr rr ref (lo, mo, ro) =  
      encodeBackward (reflect (encodeForward x lr mr rr newOff) ref) lr mr rr newOff
      : encodeSimple xs lr mr rr ref newOff
      where
        newOff = advanceRotors (lo, mo, ro) (snd lr, snd mr, snd rr)

  {- Takes tuples containing the offsets and the knock on positions and returns a tuple containing the updated offsets. -}
  advanceRotors :: Offsets -> (Int, Int, Int) -> Offsets
  advanceRotors (lo, mo, ro) (_, mknock, rknock)
    | ((26 + (ro+1)) `mod` 26) == rknock && ((26 + (mo+1)) `mod` 26) == mknock = (lo+1, mo+1, ro+1)
    | ((26 + (ro+1)) `mod` 26) == rknock = (lo, mo+1, ro+1)
    | otherwise = (lo, mo, ro+1)

  {- Takes an input Int (index of alphabet), a rotor and the rotor's offset.
     It outputs a new Int representing the forward encoded Char. -}
  applyRotor :: Int -> Rotor -> Int -> Int
  applyRotor x (cipher, _) offset = (getCharAt ((x + offset) `mod` 26) cipher - offset) `mod` 26

  {- Takes an input Int (index of alphabet), three rotors and the rotors' offsets and returns a new Int representing the fully
     forward encoded Char.
     Calls applyRotor for each rotor and offset pair (right to left). -}
  encodeForward :: Int -> Rotor -> Rotor -> Rotor -> Offsets -> Int
  encodeForward x lr mr rr (lo, mo, ro) = applyRotor (applyRotor (applyRotor x rr ro) mr mo) lr lo

  {- Takes an Int (index of alphabet) and a Reflector and applies the reflector to output a resulting Int -}
  reflect :: Int -> Reflector -> Int
  reflect x [] = x
  reflect x (y:ys)
    | alphaPos (fst y) == x = alphaPos (snd y)
    | alphaPos (snd y) == x = alphaPos (fst y)
    | otherwise = reflect x ys

  {- Takes an input Int (index of alphabet), three rotors and the rotors' offsets and returns a new Int representing the fully
     backward encoded Char.
     Calls backRotor for each rotor and offset pair (left to right). -}
  encodeBackward :: Int -> Rotor -> Rotor -> Rotor -> Offsets -> Int
  encodeBackward x (lc, _) (mc, _) (rc, _) (lo, mo, ro) = backRotor (backRotor (backRotor x lc lo) mc mo) rc ro

  {- Takes an input Int (index of alphabet), a rotor's String and the rotor's offset.
     It outputs a new Int representing the partially backward encoded Char. -}
  backRotor :: Int -> String -> Int ->  Int
  backRotor x c3 o = (getCharAt(getIndex ((x + o) `mod` 26) c3) plain - o) `mod` 26

  {- Takes an Int (index of alphabet) and a String. It converts the Int into a Char using alphaPos and finds the index where the Char
     appears in the String. -}
  getIndex :: Int -> String -> Int
  getIndex x (y:ys)
    | alphaPos y == x = 26 - length (y:ys)
    | otherwise = getIndex x ys

  {- Takes an Int (index) and a String and finds the Char in the String at the index.
     It converts the Char into an Int using alphaPos and returns this Int. -}
  getCharAt :: Int -> String -> Int
  getCharAt 0 (y:ys) = alphaPos y
  getCharAt n (y:ys) = getCharAt (n-1) ys  




{- Part 2: Finding the Longest Menu -}

  type Menu = [Int]
  type Crib = [(Char,Char)]
  type CribWithPos = [(Int, Char, Char)]

  {- Takes an input Crib and finds the longest menu by first calling getMenus on the crib after addPositions has been applied. 
     It then calls getLongest on the result of getMenus to find the longest menu. -}
  longestMenu :: Crib -> Menu
  longestMenu [] = []
  longestMenu crib
    | null crib = []
    | otherwise = getLongest [] (getMenus newCrib)
    where
      newCrib = addPositions (length crib) crib

  {- Takes a list of type a (to be compared to) and a list of lists of the same type.
     Recursively returns the longer list between the input list and the head of the list of lists until the longest is confirmed. -}
  getLongest :: [a] -> [[a]] -> [a]
  getLongest x [] = x
  getLongest x y
    | length y == 1 && length x > length (head y) = x
    | length y == 1 = head y 
  getLongest x (y:ys)
    | length x > length y = getLongest x ys
    | otherwise = getLongest y ys

  {- Takes an Int (representing how many overall positions ) and a list of tuples containing two Chars. 
     Recursively adds the position to each tuple in the list resulting in a new list of tuples of length 3 with positions added. -}
  addPositions :: Int -> Crib -> CribWithPos
  addPositions _ [] = []
  addPositions n (x:xy) = (n - length (x:xy), fst x, snd x) : addPositions n xy

  {- Takes a list of tuples (crib with positions).
     Returns a list of the longest menus from each starting position. 
     Uses list comprehension to apply findMenu to each position. 
     Uses fst3 and trd3 to access the first and third values in the tuples. -}
  getMenus :: CribWithPos -> [[Int]]
  getMenus [] = []
  getMenus x = [findMenu (fst3 n) (trd3 n) x [] | n <- x]

  {- Takes an Int (start position), a Char (start Char), a tuple (crib with positions) and a list of Ints (filter).
     Finds possible options from the start position/Char through findOptions. 
     Then filters already visited positions through filterOptions. 
     Recursively follows the chain until there are no unexplored options and returns the list of positions in order visited. 
     Uses fst3 and trd3 to access the first and third values in the tuples. -}
  findMenu :: Int -> Char -> CribWithPos -> [Int] -> [Int]
  findMenu _ _ [] _ = []
  findMenu cPos cChar crib filter
    {- If more than 1 option after filter, it calls getLongest on the list of findMenu calls -}
    | length options > 1 = cPos : getLongest [] [findMenu (fst3 n) (trd3 n) crib (cPos : filter) | n <- options]
    | length options == 1 = cPos : findMenu (fst3 (head options)) (trd3 (head options)) crib (cPos : filter)
    | null options = [cPos]
    where options = filterOptions (findOptions cPos cChar crib) filter
  

  {- Takes an Int (start position), a Char (start Char) and a list of tuples (crib with positions).
     Uses list comprehension to produce a list of tuples which contain the start Char in its middle value.
     Returns this reduced list of tuples.
     Uses fst3 and snd3 to access the first and second values in the tuples.  -}
  findOptions :: Int -> Char -> CribWithPos -> CribWithPos
  findOptions _ _ [] = []
  findOptions cPos x crib = [n | n <- crib, snd3 n == x && fst3 n /= cPos]

  {- Takes a list of tiples (crib with positions) and a list of Ints (filter).
     Returns the list of tuples where the first value of the tuple does not appear the in filter list.
     If, after calling isIn, the result is True then the tuple will be filtered from the list. 
     Uses fst3 to access the first value in the tuples. -}
  filterOptions :: CribWithPos -> [Int] -> CribWithPos
  filterOptions [] _ = []
  filterOptions x [] = x
  filterOptions init filter = [x | x <- init, not (isIn (fst3 x) filter)]

  {- Takes an input of type a and a list of the same type.
     Recursively checks if the singular input is equal to the head of the list until all combinations have been checked.
     If none of the combinations are equal, the function will return False, else it will return True. -}
  isIn :: Eq a => a -> [a] -> Bool
  isIn n [] = False
  isIn n x
    | length x == 1 && head x == n = True
    | length x == 1 = False
  isIn n (x:xs)
    | x == n = True
    | otherwise = isIn n xs

  {- Set of functions to use my addPositions output. Each take an input Tuple (crib with position).
     Each will return the first, second or third value in the tuple depending on which function is used. -}
  fst3 :: (Int, Char, Char) -> Int
  fst3 (x,_,_) = x
  snd3 :: (Int, Char, Char) -> Char
  snd3 (_,x,_) = x
  trd3 :: (Int, Char, Char) -> Char
  trd3 (_,_,x) = x




{- Part 3: Simulating the Bombe -}
  
  {- Takes an input Crib and produces either Nothing or a tuple containing the offsets and stecker required to decode a message 
     Does this by calling tryAllRotors on the crib with positions added, the longest menu, starting Char, and all possible initial Steckers -}
  breakEnigma :: Crib -> Maybe (Offsets, Stecker)
  breakEnigma [] = Nothing
  breakEnigma x = tryAllRotors newCrib menu getAllRotors allInitStecks
    where 
      newCrib = addPositions (length x) x
      menu = longestMenu x
      startChar = snd3 (head [n | n <- newCrib, fst3 n == head menu]) 
      allInitStecks = getAllInitStecks startChar

  {- Takes an Int (how many rotors), a set of Offsets and their knock-on addPositions and produces a list of incrementing rotors 
     recursively calls advanceRotors until the input Int is decremented to 0 -}
  rotorsForCombo :: Int -> Offsets -> (Int, Int, Int) -> [Offsets]
  rotorsForCombo n init knocks
    | n == 0 = []
    | otherwise = init : rotorsForCombo (n-1) newOffs knocks
    where
      newOffs = advanceRotors init knocks

  {- Takes an Int (current position) and a list of Offsets and returns the Offsets for the given position
     Recursively calls itself until the input Int is decremented to 0 -}
  rotorForCombo :: Int -> [Offsets] -> Offsets
  rotorForCombo 0 x = head x
  rotorForCombo n [x] = x
  rotorForCombo n (x:xs) = rotorForCombo (n-1) xs

  {- A list of Offsets containing all possible combinations -}
  getAllRotors = rotorsForCombo 17576 (0, 0, 0) (17, 5, 22)

  {- Takes a Char and produces a list containing all tuples of possible pairs of letters
     e.g. if Char was 'A' =  [('A','A'),('A','B'), ... , ('A','Z')] 
     Makes use of convertBack -}
  getAllInitStecks :: Char -> [(Char, Char)]
  getAllInitStecks x = [(x, n) | n <- convertBack [0..25]]

  {- Takes an Int (index of alphabet), a set offsets and a Stecker and produces a steckerEncoded Int without applying the stecker at the end
     Calls encodeSimple with rotor1 rotor2 rotor3 reflectorB on the steckered Int -}
  bombeEncode :: Int -> Offsets -> Stecker -> Int
  bombeEncode x offs steck = head (encodeSimple [n] rotor1 rotor2 rotor3 reflectorB offs)
    where
      n = reflect x steck

  {- Takes a CribWithPos, its longest menu, a list of Offsets, a list of possible initial Steckers and produces either 
     Nothing or a tuple containing the offsets and stecker required to decode a message
     This function recursively goes through the list of Offsets until tryAllStecks produces a value that isn't Nothing -}
  tryAllRotors :: CribWithPos -> Menu -> [Offsets] -> [(Char, Char)] -> Maybe (Offsets, Stecker)
  tryAllRotors _ _ [] _ = Nothing
  tryAllRotors crib menu (x:xs) allInitStecks
    | isJust tryOffs = tryOffs
    | otherwise = tryAllRotors crib menu xs allInitStecks
    where
      tryOffs = tryAllStecks crib menu x allInitStecks

  {- Takes a CribWithPos, its longest menu, a set of Offsets, a list of possible initial Steckers and produces either 
     Nothing or a tuple containing the offsets and stecker required to decode a message
     This function produces a list of all attempts resulting from the Offsets and each initial Stcecker using tryCombo
     It then filters all Nothing values and returns Nothing if the resulting list is empty or returns the first none Nothing value -}
  tryAllStecks :: CribWithPos -> Menu ->  Offsets -> [(Char, Char)] -> Maybe (Offsets, Stecker)
  tryAllStecks _ _ _ [] = Nothing
  tryAllStecks crib menu offs initStecks 
      | null filteredAttempts = Nothing
      | otherwise = head filteredAttempts
    where 
      attempts = [tryCombo crib menu offs (rotorsForCombo (length crib) offs (17,5,22)) [x] | x <- initStecks]
      filteredAttempts = [x | x <- attempts, isJust x]

  {- Takes a CribWithPos, its longest menu, a set of Offsets, a list of Offsets for each position, a Stecker and produces either 
     Nothing or a tuple containing the offsets and stecker required to decode a message 
     Iterates through the menu and recursively uses checkSteck to see if there are any overlaps in the Stecker and if the menu becomes empty
     the function will return the tuple containing the Offsets and Stecker. Else it will return Nothing -}
  tryCombo :: CribWithPos -> Menu -> Offsets -> [Offsets] -> Stecker -> Maybe (Offsets, Stecker)
  tryCombo _ [] offs _ steck = Just (cleanOffsets offs, steck)
  tryCombo crib (x:xs) offs allOffs steck
    | checkSteck (output, cipher) steck == steck = tryCombo crib xs offs allOffs ((output, cipher) : steck)
    | otherwise = Nothing
    where 
      input = snd3 (head [n | n <- crib, fst3 n == x])
      offsetsAtPos = rotorForCombo x allOffs
      output = head (convertBack [bombeEncode (alphaPos input) offsetsAtPos steck])
      cipher = trd3 (head [n | n <- crib, fst3 n == x])

  {- Takes a set of Offsets and returns the same Offsets with `mod` 26 applied to each value -}
  cleanOffsets :: Offsets -> Offsets
  cleanOffsets (lo, mo, ro) = (lo `mod` 26, mo `mod` 26, ro `mod` 26)

  {- Takes a tuple containing two Chars and removes any tuples in a Stecker that contains just one of the Chars -}
  checkSteck :: (Char, Char) -> Stecker -> Stecker
  checkSteck _ [] = []
  checkSteck (output, cipher) steck = 
    [n | n <- steck, (fst n /= output && snd n /= output && fst n /= cipher && snd n /= cipher) || (n == (output, cipher) || n == (cipher, output))]


  makeCrib :: String -> String -> Crib
  makeCrib _ [] = []
  makeCrib [] _ = []
  makeCrib [] [] = []
  makeCrib (x:xs) (y:ys)
      | length (x:xs) == length (y:ys) = (x, y) : makeCrib xs ys






  

{- Useful definitions and functions -}

   -- substitution cyphers for the Enigma rotors
   -- as pairs of (wirings, knock-on position)
   -- knock-on position is where it will cause the next left wheel to
   -- advance when it moves past this position
 
        --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  plain="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
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


  {- alphaPos: given an uppercase letter, returns its index in the alphabet
     ('A' = position 0; 'Z' = position 25)
   -}
  alphaPos :: Char -> Int
  alphaPos c = (ord c) - ord 'A'

  bombeTestText = "CALLMEISHMAELSOMEYEARSAGONEVERMINDHOWLONGPRECISELYHAVINGLITTLEORNOMONEYINMYPURSEANDNOTHINGPARTICULARTOINTERESTMEONSHOREITHOUGHTIWOULDSAILABOUTALITTLEANDSEETHEWATERYPARTOFTHEWORLDITISAWAYIHAVEOFDRIVINGOFFTHESPLEENANDREGULATINGTHECIRCULATIONWHENEVERIFINDMYSELFGROWINGGRIMABOUTTHEMOUTHWHENEVERITISADAMPDRIZZLYNOVEMBERINMYSOULWHENEVERIFINDMYSELFINVOLUNTARILYPAUSINGBEFORECOFFINWAREHOUSESANDBRINGINGUPTHEREAROFEVERYFUNERALIMEETANDESPECIALLYWHENEVERMYHYPOSGETSUCHANUPPERHANDOFMETHATITREQUIRESASTRONGMORALPRINCIPLETOPREVENTMEFROMDELIBERATELYSTEPPINGINTOTHESTREETANDMETHODICALLYKNOCKINGPEOPLESHATSOFFTHENIACCOUNTITHIGHTIMETOGETTOSEAASSOONASICANTHISISMYSUBSTITUTEFORPISTOLANDBALLWITHAPHILOSOPHICALFLOURISHCATOTHROWSHIMSELFUPONHISSWORDIQUIETLYTAKETOTHESHIPTHEREISNOTHINGSURPRISINGINTHISIFTHEYBUTKNEWITALMOSTALLMENINTHEIRDEGREESOMETIMEOROTHERCHERISHVERYNEARLYTHESAMEFEELINGSTOWARDSTHEOCEANWITHME"
  encodedBombeTestText = "IEKMVRCIERGNRGFEWBTFTISNFJAUADRLANCKIVUBTKCHYLTUGHKNMNJVXSBUNVSORHALMQVGOJUXQJCVJBIVUQDPFHQTKERKQGWIFASRCCDOZIKYIFKYHILRNDSJIGJRDPYYQHHYAHYQVNYKPWCDALBKVKFDJDPHUFFQTNGHAIRAOMEQANNZJHLFLIXZHZMMZWNVZWHBWNJIMHEONFYSUFGMZCVINEPFSNZNDFLTIVIDVXRZNILTKGSLURGJATWIBISVNUZSHAMMDCXULKDKKKWWBVKJWTQKUGWJALGKZQWSIYBOBOVRUZEAWFUKVWTZNJVOXLNFSAZVGBYEUTJWOMYIJYFVHUYPJQQSJRSJIIVYMGYZTPPUIUDKRLHLFSCHLEFPNVNLBDOMATGNWCICYFTTPMYXOXGUETPQYCEVRJBFPMGLBIFOVYHDIRXCRUJTNJUMRLTBFUDZLRHUTEMHJPYFDXOWVRWTUQYIDXGJCJUAHZTQJTXLRMGHOBGMGASNHROAKPNWZQAFTQUDHVCFGDAHOSJZRKYBGEXYQXKEDGQUJXPBUPTUMXRHBMWWLIWAVQFWBLYTFXEIFCOAVZJEOVZYJBLURZLRDZSNOXJELHDDHRWVSQMILAMLJOLRCGVFPYYUKRZQJPNIWZRPQUVGVGITIGKJNSEZZFZACZUDEGBJJHBQWTHLDHHSRWCNHGXQZFTFTWWJADTREONGMARCBKYZLNPVXQZVSAQFMINGHNWELLBVWCWAMJAPFIAYNXXJJOKJSGSCTGAPDPZCAFAMKBNHENFZHIGXWXAKMARPXDIPACTDZLHTCSDHKZBXJFXZMHVATQHBXXCLHBOUPCROITBSCNMZZQVNTRHOAKUZTFMRJANOVJHQ"

  bombeTestCrib = [('C','I'),('A','E'),('L','K'),('L','M'),('M','V'),('E','R'),('I','C'),('S','I'),('H','E'),('M','R'),('A','G'),('E','N'),('L','R'),('S','G'),('O','F'),('M','E'),('E','W'),('Y','B'),('E','T'),('A','F'),('R','T'),('S','I'),('A','S'),('G','N'),('O','F'),('N','J'),('E','A'),('V','U'),('E','A'),('R','D'),('M','R'),('I','L'),('N','A'),('D','N'),('H','C'),('O','K'),('W','I'),('L','V'),('O','U'),('N','B'),('G','T'),('P','K'),('R','C'),('E','H'),('C','Y'),('I','L')]
