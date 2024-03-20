module QuizMarker where

import Data.Maybe(isJust, listToMaybe)
import Data.List(inits,nub)
import Data.Char(isSpace,isDigit)
import Data.Foldable(find)
import Text.Read(readMaybe)
import Data.Time.Format
import Data.Time.Clock
import Test.QuickCheck



{- A `Parser a` consumes input (of type `String`),
   and can either fail (represented by returning `Nothing`)
   or produce a value of type `a` along with a `String`
   of leftovers -- this is the unconsumed suffix
   of the original input.

   Side note:
    This combines features of both the Maybe monad and the
    State monad. So why didn't we just use those?
    Well, note that  `State String (Maybe a)`
    isn't quite what we want (because then failure
    could consume input), and neither is
    `Maybe (State String a)` (because then failure
    could not depend on what the input is).
 -}
newtype Parser a = Parser (String -> Maybe (String,a))

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (fmap f) . p

instance Applicative Parser where
  pure x = Parser $ \s -> Just (s,x)
  Parser f <*> Parser x =
     Parser $ \s ->
        do -- this is using the Maybe monad
          (s',f') <- f s
          fmap f' <$> x s'

{- `>>=` can be used to compose two parsers
   sequentially, so that the second parser
   operates on the leftovers of the first.
   If either parser fails, the composite
   parser also fails.

   For example, the following parser:

     parseDouble >>= \x ->
     keyword "+" >>
     parseDouble >>= \y ->
     return(x+y)

   or equivalently in do notation:

     do
       x <- parseDouble
       keyword "+"
       y <- parseDouble
       return(x+y)

   should return 3.0::Double when given
   the input string "1+2"
 -}
instance Monad Parser where
  return = pure
  Parser m >>= k =
    Parser $ \s ->
      do -- this is using the Maybe monad
        (s',a) <- m s
        let Parser m' = k a
        m' s'

{- A parser that always fails
 -}
abort :: Parser a
abort = Parser $ const Nothing

{- `try m` is a parser that fails if
   m is Nothing, and otherwise succeeds
   with result `m` without consuming any
   input.
 -}
try :: Maybe a -> Parser a
try Nothing  = abort
try (Just a) = return a

{- `keyword s` is a parser
   that consumes a string s,
   and fails if unable to do so.
 -}
keyword :: String -> Parser ()
keyword kw =
  Parser(\s ->
           let (xs,ys) = splitAt (length kw) s
           in
             if xs == kw then
               Just (ys,())
             else
               Nothing)

{- `parsePred f` is a parser
   that consumes and returns the
   longest prefix of the input
   such that every character satisfies
   `p`. Never fails.
 -}
parsePred :: (Char -> Bool) -> Parser String
parsePred f =
  Parser(\s ->
           let xs = takeWhile f s
           in
             Just (drop (length xs) s, xs))

{- `least f` consumes and returns the
   shortest prefix of the input that
   satisfies f.

   Fails if no prefix of the input
   satisfies f.
 -}
least :: (String -> Bool) -> Parser String
least f =
  Parser(\s -> do
            xs <- find f $ inits s
            return(drop (length xs) s, xs))

{- `orelse p1 p2` is a parser that
   behaves like p1, unless p1 fails.
   If p1 fails, it behaves instead like p2.
 -}
orelse :: Parser a -> Parser a -> Parser a
orelse (Parser p1) (Parser p2) =
  Parser $ \s ->
    case p1 s of
      Nothing -> p2 s
      res -> res

{- `parseWhile p` will repeatedly run
   the parser p until p fails,
   and return the list of all
   results from the successful
   runs in order.

   For example, we would expect:

   runParserPartial (parseWhile (keyword "bla")) "blablably"
   == Just ("bly",[(),()])

   Note that even if `p` fails,
   `parseWhile` should not fail.
 -}
parseWhile :: Parser a -> Parser [a]
parseWhile p =
  do
    x <- p
    xs <- parseWhile p
    return (x:xs)
  `orelse` return []

{- `first ps` behaves as the first parser
   in `ps` that does not fail. If they all
   fail, `first ps` fails too.
 -}
first :: [Parser a] -> Parser a
first [] = abort
first (p:ps) = orelse p (first ps)

-- first ps = foldl orelse p:ps
-- first ps = orelse p

{- peekChar is a parser that
   returns the first character
   of input without consuming it,
   failing if input is empty.

   For example, we'd expect

     runParserPartial peekChar "bla"
     == Just ("bla",'b')
 -}
peekChar :: Parser Char
peekChar = Parser $ \x ->
  case x of
    [] -> Nothing
    x -> Just (x, head x)


{- parseChar is a parser that
   consumes (and returns) a single
   character, failing if there
   is nothing to consume.

   For example, we'd expect

     runParserPartial parseChar "bla"
     == Just ("la",'b')
 -}
parseChar :: Parser Char
parseChar = Parser $ \x ->
  case x of
    [] -> Nothing
    (w:ws) -> Just (ws, w)


{- A parser that consumes all leading
   whitespace (as determined by isSpace).
   Should always succeed.
 -}
whiteSpace :: Parser ()
whiteSpace = (parsePred isSpace >> return ())

{- parseBool either
   consumes "true" to produce True,
   consumes "false" to produce False,
   or fails if unable.
 -}
parseBool :: Parser Bool
parseBool = orelse (keyword "true" >> return True) (keyword "false" >> return False)

{- parsePositiveInt is a parser that parses a
   non-empty sequence of digits (0-9)
   into a number.
 -}
parsePositiveInt :: Parser Int
parsePositiveInt = do
  d <- parsePred isDigit
  case readMaybe d of
    Just n | n > 0 -> return n
    _ -> abort

{- parseDouble is a parser that parses a number on the
   format:

     -<digits>.<digits>

   Where the minus sign and decimal point are optional.
   Should fail if the start of input does not match
   this format.

   The JSON file format also allows numbers with E notation.
   We do not support such numbers.
 -}

parseDouble :: Parser Double
parseDouble = do
  negative <- parseOptional (fmap (const True) (keyword "-")) False
  digits <- parsePred isDigit
  decimal <- parseOptional (parseChar >> parsePred isDigit) ""
  let numStr = if null decimal then digits else digits ++ "." ++ decimal
  case readMaybe numStr of
    Just n -> return $ if negative then -n else n
    Nothing -> abort

parseOptional :: Parser a -> a -> Parser a
parseOptional (Parser p) b = Parser $ \s ->
  case p s of
    Just (s', a) -> Just (s', a)
    Nothing -> Just (s, b)


{- `parseString` is a parser that consumes a quoted
   string delimited by " quotes, and returns it.

   For example, we would expect:

     runParserPartial parseString "\"ab\"r"
     == runParserPartial parseString ['"','a','b','"','r']
     == Just ("r","ab")

   To add an additional complication, it's possible
   for the input string to contain escape sequences.
   Note in particular that escaped quotes do not end
   the string. 
   For example:

     runParser parseString "\"a\\\"b\""
     Just "a\"b"

   Hint: what does (readMaybe s)::Maybe String do?
         And how is that useful here?
         And how is what (readMaybe "[]")::Maybe String
         does less than optimally useful?
 -}
parseString :: Parser String
parseString = do
  keyword "\""
  str <- parseWhile (orelse escapedHelper regHelper)
  keyword "\""
  let unescapedStr = concatMap unescape str
  return unescapedStr
  where
    escapedHelper :: Parser String
    escapedHelper = do
      keyword "\\"
      char <- parseChar
      return ['\\', char]
    regHelper :: Parser String
    regHelper = do
      char <- parseChar
      if elem char "\\\""
        then abort
        else return [char]
    unescape :: String -> String
    unescape ('\\':'\"':xs) = '\"' : unescape xs
    unescape (x:xs) = x : unescape xs
    unescape [] = []

testsBoolString :: Bool
testsBoolString = u1 && u2 && u3 && u4 && u5 && u6 && u7 && u8 && u9
  where
    u1 = runParserPartial parseBool "hellotrue" == Nothing
    u2 = runParserPartial parseBool "truehello" == Just ("hello", True)
    u3 = runParserPartial parseBool "ttruehello" == Nothing
    u4 = runParserPartial  parseString "\"safsasfa\"sfaf\"\"afasfasf" == Just ("sfaf\"\"afasfasf", "safsasfa")
    u5 = runParserPartial  parseString "\"safsasfa\\\"sfaf\"\"afasfasf" == Just("\"afasfasf", "safsasfa\"sfaf")
    u6 = runParser parseString "\"a\\\"b\"" == Just "a\"b"
    u7 = runParser parseString "[]" == Nothing
    u8 = runParser parseString "['a']" == Nothing
    u9 = runParserPartial parseString "[]a" == Nothing

{- `parseList l r p` parses a
   comma-separated list that
   is delimited on the left by l,
   on the right by r,
   and where p is a parser for single
   elements of the list type.

   For example, we would expect:

     runParser (parseList '[' ']' parseDouble) "[1, 2]"
     == Just [1.0,2.0]

   Trailing commas, or omitted commas between elements,
   should yield failure.

   You should accept any amount of whitespace
   after l, before r, and before and after comma,
   including no whitespace. So we would expect
   the same result as above for the following call:

     runParser (parseList '[' ']' parseDouble) "[ 1  ,2 ]"

   The fact that we haven't hardcoded [ and ] as the
   delimiters will be handy later.
 -}
parseList :: Char -> Char -> Parser a -> Parser [a]
parseList left right p = do
  keyword [left]
  whiteSpace
  xs <- (p `sepBy` (whiteSpace >> keyword "," >> whiteSpace))
  whiteSpace
  keyword [right]
  return xs

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = do
  x <- p
  xs <- parseWhile $ sep >> p
  return (x:xs)

unitTests1 :: Bool
unitTests1 = u1 && u2 && u3 && u4 && u7 && u8
  where
    u1 = runParserPartial (parseList '[' ']' parseDouble) "[2.0, 3, 4, 6a]" == Nothing
    u2 = runParserPartial(parseList '[' ']' abort::Parser String) "[a, b, c, d]" == Nothing
    u3 = runParserPartial (parseList '[' ']' whiteSpace) "[     a,   b,c,d, e]" == Nothing
    u4 = runParserPartial (parseList '[' ']' (parsePred (\x -> x == 'a'))) "[aaaaaa, a, aa, aa, a, a]" == Just("", ["aaaaaa", "a", "aa", "aa", "a", "a"])
    -- u5 = runParserPartial (parseList '[' '1' parseDouble) "[1.01" == Just (".01",[])
    -- u6 = runParser (parseList '[' ']' parseDouble) "[]" == Just []
    u7 = runParserPartial parsePositiveInt "12hdfgn4" == Just ("hdfgn4",12)
    u8 = runParserPartial parsePositiveInt "0" == Nothing

unitTests2 :: Bool
unitTests2 = u1 && u2 && u3 && u4 && u5 && u6 && u7 && u8 && u9
  where
    u1 = runParserPartial parsePositiveInt "0" == Nothing
    u2 = runParserPartial parsePositiveInt "012" == Just ("", 12)
    u3 = runParserPartial parseDouble "1..0" == Just ("..0",1.0)
    u4 = runParserPartial parseDouble "1.as" == Just (".as",1.0)
    u5 = runParserPartial parseDouble "-.123" == Nothing
    u6 = runParserPartial parseDouble ".123" == Nothing
    u7 = runParserPartial parseDouble "1." == Just (".",  1.0)
    u8 = runParserPartial parseDouble "." == Nothing
        && runParserPartial parseDouble "-" == Nothing
        && runParserPartial parseDouble "-." == Nothing
        && runParserPartial parseDouble "." == Nothing
    u9 = runParserPartial parseDouble "1.1.1" == Just (".1", 1.1)

unitTests3 :: Bool
unitTests3 = u1 && u2 && u3 && u4 && u5 && u6 && u7 && u8 && u9
  where
    u1 = runParserPartial parseBool "hellotrue" == Nothing
    u2 = runParserPartial parseBool "truehello" == Just ("hello", True)
    u3 = runParserPartial parseBool "ttruehello" == Nothing
    u4 = runParserPartial  parseString "\"safsasfa\"sfaf\"\"afasfasf" == Just ("sfaf\"\"afasfasf", "safsasfa")
    u5 = runParserPartial  parseString "\"safsasfa\\\"sfaf\"\"afasfasf" == Just("\"afasfasf", "safsasfa\"sfaf")
    u6 = runParser parseString "\"a\\\"b\"" == Just "a\"b"
    u7 = runParser parseString "[]" == Nothing
    u8 = runParser parseString "['a']" == Nothing
    u9 = runParserPartial parseString "[]a" == Nothing

unitTests4 :: Bool
unitTests4 = u1 && u2 && u3 && u4 && u5 && u6 && u7 && u8 && u9
  where
    u1 = runParserPartial (parseList '[' ']' parseDouble) "[12 13]" == Nothing
    u2 = runParserPartial (parseList '[' ']' parseDouble) "[1,,2]" == Nothing
    u3 = runParserPartial (parseList '[' ']' parseDouble) "[,1,2]" == Nothing
    u4 = runParserPartial (parseList '[' ']' parseDouble) "[1,2,]" == Nothing
    u5 = runParserPartial (parseList '[' '1' parseDouble) "[1, 2]" == Just (", 2]", [])
    u6 = runParserPartial (parseList '[' ']' parseDouble) "[12, 13" == Nothing
    u7 = True
    u8 = True
    u9 = True


{- `runParser s p` runs the parser p
   on input s.
   This should return Nothing if:
   - the parser fails, or
   - the parser succeeds, but has not
       consumed all input.
 -}
runParser :: Parser a -> String -> Maybe a
runParser (Parser p) s =
  case p s of
    Just ([],a) -> Just a
    _           -> Nothing

{- `runParserPartial s p` runs the parser p
   on input s.

   In the event that some of the input remains
   unconsumed, it is returned alongside the result.

   You should only use this for testing ---
   input that contains spare tokens at the end
   should be considered malformed, which is
   what `runParser` handles.
 -}
runParserPartial :: Parser a -> String -> Maybe(String,a)
runParserPartial (Parser p) s = p s

{- Now for our JSON parser.
   `JSON` is a datatype for representing
   JSON objects, which are key-value pairs
   mapping names to values.
 -}
data JSON = JSON [(String,Data)] deriving (Eq,Show)

{- Values in a JSON object can be of the following types:
   - a floating-point number
   - a string
   - a list of data objects, not necessarily of the same type
   - a boolean
   - null
   - another JSON object.
 -}
data Data =
  Number Double
  | String String
  | List [Data]
  | Bool Bool
  | Null
  | JSONData JSON deriving (Eq,Show)

{- This Arbitrary instance only generates keys in the
    range a..z.
   This is for readability, and should not be construed
   as a limitation on the type of keys allowed.

   The generator is deliberately biased towards
   smaller JSON objects.
 -}
instance Arbitrary JSON where
  arbitrary = JSON <$> listOf ((,) <$> key <*> arbitrary)
    where key = listOf $ elements ['a'..'z']
  shrink (JSON d) =
    JSON <$> shrinkList (const []) d

instance Arbitrary Data where
  shrink (JSONData j) = JSONData <$> shrink j
  shrink (List l) = List <$> shrink l
  shrink d = []  
  arbitrary = frequency [
                     (3,Number   <$> arbitrary),
                     (3,String   <$> arbitrary),
                     (1,List     <$> smaller arbitrary),
                     (3,Bool     <$> arbitrary),
                     (1,JSONData <$> smaller arbitrary),
                     (1,pure Null)] where
    smaller g = sized (\n -> resize (n `div` 4) g)

{- A parser for JSON objects.

   JSON objects are {}-delimited,
   comma-separated lists of key-value
   pairs.

   A key-value pair is a "-delimited string
   and a data value, separated by a :

   Allow any amount of whitespace
   before or after the elements

     { } : ,

   Hints:
    Because JSON objects can themselves be data, this
    function needs to be mutually recursive with parseData.
 -}

parseJSON :: Parser JSON
parseJSON = do
  keyvals <- parseList '{' '}' getPair
  return $ JSON keyvals

getPair :: Parser (String, Data)
getPair = do
  key <- parseString
  whiteSpace
  keyword ":"
  whiteSpace
  value <- parseData
  return (key, value)

{- A parser for JSON data values.

   Hints:
    You've already done most of the work for this in the
    previous functions, it's just a matter of composing
    them.

    Because JSON objects can themselves be data, this
    function needs to be mutually recursive with parseJSON.
 -}
parseData :: Parser Data
parseData = first
  [ Number <$> parseDouble, String <$> parseString, List <$> parseList '[' ']' parseData, 
  Bool <$> parseBool, keyword "null" >> return Null, JSONData <$> parseJSON]

--- TESTING ---
mcJSONRecipe :: Maybe JSON
mcJSONRecipe = runParser parseJSON "{\"type\": \"minecraft:crafting_shaped\",\"pattern\": [\"X\",\"#\"],\"key\": {\"#\": {\"item\": \"minecraft:granite\"},\"X\": {\"item\": \"minecraft:stick\"}},\"result\": {\"item\": \"examplemod:remote_lever_block\"}}"

submsJSON :: Maybe JSON
submsJSON = runParser parseJSON "{\"z1345678\":{\"session\":\"23T2\",\"quiz_name\":\"quiz01\",\"student\":\"Jean-Baptiste Bernadotte\",\"answers\":[[4],[2],[2],[1],[2],[1,2],[1,3],[1,2,3,4,5]],\"time\":\"2023-06-02 23:13:13\"},\"z2745678\":{\"session\":\"23T2\",\"quiz_name\":\"quiz01\",\"student\":\"Hrafna-Flóki Vilgerðarson\",\"answers\":[[1],[],[1]],\"time\":\"2023-06-02 12:16:52\"}}"

sampleSubmsnToSubmissions :: Maybe [(String,Submission)]
sampleSubmsnToSubmissions = do
  jsn <- submsJSON
  outp <- toSubmissions jsn
  return outp

tryParseMcToSubmissions :: Maybe [(String, Submission)]
tryParseMcToSubmissions = do
  mc <- mcJSONRecipe
  subms <- toSubmissions mc
  return subms

tryParseMcToSubmission :: Maybe Submission
tryParseMcToSubmission = do
  mc <- mcJSONRecipe
  subm <- toSubmission mc
  return subm

prop_minecraftNotSubmission :: Bool
prop_minecraftNotSubmission = (tryParseMcToSubmission == Nothing) && (tryParseMcToSubmissions == Nothing) 

prop_submissionsParses :: Bool
prop_submissionsParses = sampleSubmsnToSubmissions /= Nothing

{- Time strings are represented in the following format:

     YYYY-MM-DD HH-MM-SS

   where H is in 24h format. This is a string-to-time
   function that accepts the above format.
 -}
toTime :: String -> Maybe UTCTime
toTime = parseTimeM True defaultTimeLocale "%F %X"

{- A quiz submission consists of:

  - A session (something like 23T2)
  - Name of the quiz (something like quiz02)
  - Name of the student (something like "Jean-Baptiste Bernadotte")
  - A list of answers.
    For example, a student who answer option 4 on question 1,
    and options 1,3 on question 2,
    would have [[4],[1,3]]
  - A submission time.
 -}
data Submission =
  Submission { session :: String,
               quizName :: String,
               student :: String,
               answers :: [[Int]],
               time :: UTCTime
             } deriving (Eq,Show)

instance Arbitrary Submission where
  arbitrary =
    Submission <$>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitraryAnswers <*>
    arbitraryTime where arbitraryAnswers = map (map getPositive) <$> arbitrary

-- Likes generating years in the far future.
arbitraryTime :: Gen UTCTime
arbitraryTime =
  do
    Large d <- arbitrary
    t <- choose (0,86400)
    return $ UTCTime (toEnum $ abs d) (secondsToDiffTime t)
          

{- These utility functions will be handy I promise -}
getString :: Data -> Maybe String
getString (String s) = return s
getString _ = Nothing

getList :: Data -> Maybe [Data]
getList (List xs) = return xs
getList _ = Nothing

getNumber :: Data -> Maybe Double
getNumber (Number n) = return n
getNumber _ = Nothing

getJSON :: Data -> Maybe JSON
getJSON (JSONData j) = return j
getJSON _ = Nothing

{- This function should convert a JSON object
   to a `Submission`.
   This should fail if:
   - Either of the keys
       session, quiz_name, student, answers,time
     are absent from the JSON object
   - Either of the above are present, but
     do not have the expected type.
     All fields except `answers` are expected to
     hold string values; `answers` should hold
     a list of lists of numbers.
   - The `time` key is present, and holds
     a string, but this string is not
     recognised as a valid time by
     `toTime`.
   - The `answers` contain numbers that are
     not whole, or not positive.

   You should *not* fail if the JSON object
   contains more keys than the above.
   Politely ignore such keys.

   If either of the above keys have duplicate
   entries in the JSON object, you're free
   to do anything you want.
   Duplicates of other keys should be ignored.
 -}
toSubmission :: JSON -> Maybe Submission
toSubmission (JSON json) = do
  session <- lookup "session" json >>= getString
  quizName <- lookup "quiz_name" json >>= getString
  student <- lookup "student" json >>= getString
  answersData <- lookup "answers" json >>= getList
  timeData <- lookup "time" json >>= getString
  answers <- mapM answersHelper answersData
  time <- toTime timeData
  return $ Submission session quizName student answers time

answersHelper :: Data -> Maybe [Int]
answersHelper x = getList x >>= mapM (fmap round . getNumber)

{- This function should convert a JSON object
   to a key-value store where the values are
   of type `Submission` instead of JSON objects.

   Should fail if the JSON object holds one or
   more values that are not valid submissions.
 -}
toSubmissions :: JSON -> Maybe [(String,Submission)]
toSubmissions (JSON json) = error "TODO toSubmissions"
  
  -- do
  -- fmap (a, b) = ((,) a) 
  -- getJSON b >>= toSubmission


{- There are two kinds of questions:
   - multiple-choice, represented by CheckBox
   - single-choice, represented by Radio
 -}
data QuestionType = Radio | CheckBox deriving (Eq,Show)

instance Arbitrary QuestionType where arbitrary = elements [Radio,CheckBox]

{- A question has:
   - a question number
   - a question type
   - a list of correct answers
 -}
data Question =
  Question { number  :: Int,
             qtype   :: QuestionType,
             correct :: [Int]
           } deriving (Eq,Show)

{- A quiz comprises a deadline and a list of questions.
 -}
data Quiz =
  Quiz { deadline :: UTCTime,
         questions :: [Question]
       } deriving (Eq,Show)

instance Arbitrary Question where
  arbitrary = do
    Positive n <- arbitrary
    qtype <- arbitrary
    correct <- listOf1 (getPositive <$> arbitrary)
    return $ Question n qtype correct

nubBy :: Eq b => (a -> b) -> [a] -> [a]
nubBy f [] = []
nubBy f (x:xs) = x:nubBy f (filter ((/= f x) . f) xs)

{- This generator is set up to generate quizzes with distinct
   question numbers starting from 1.

   This partially overrides the behaviour of the Question
   generator.
 -}
instance Arbitrary Quiz where
  arbitrary = Quiz <$> arbitraryTime <*> arbitraryQuestions
    where
      arbitraryQuestions = do
        qs <- arbitrary
        return $ map (\(n,q) -> Question n (qtype q) (correct q)) $ zip [1..] qs

{- `parseQuiz` is a parser that
   reads a quiz from an input representing
   a key. A key is a file such that:

   - The first line is a date, of the form
     accepted by toTime
   - The subsequent lines represent questions,
     and have the form

       n|type|correct

     Where `n` is a question number,
     `type` is either a checkbox or a radio button,
     and `correct` is a comma-separated list
     of answers with no delimiters.
     The answer must be a a list of non-empty
     positive integers denoting
     the correct alternatives.

   Malformed input should be rejected.
   Questions must start from 1, be consecutive,
   be positive integers,
   and be free of duplicates question numbers;
   reject any input that does not conform.
   Answers must be positive integers,
   and there must be at least one answer.
 -}
parseQuiz :: Parser Quiz
parseQuiz = error "TODO: implement parseQuiz"

{- And now for the business logic!

   `markQuestion q as` should assign marks
   to the answers `as` given to question `q`
   as follows:

   - If the question is single-choice,
     give 1 mark if the student supplied
     exactly one answer, which is also
     correct.
   - If the question is multiple-choice,
     give marks according to the
     following formula:

       max(0,(right - wrong)/correct)

     Where `right` is the number of correct
     answers in `as`, `wrong` is the number
     of incorrect answers in `as`,
     and `correct` is the number of correct
     answers in the answer sheet.

     Allow for the possibility that either
     the given set of answers, or the
     answer sheet, may contain duplicates.
     Duplicates should be ignored
     for purposes of the above tally.
 -}
markQuestion :: Question -> [Int] -> Double
markQuestion = error "TODO: implement markQuestion"

{- The mark assigned to a quiz submission is:
   - 0 if submitted after the deadline.
   - the sum of the marks for each question,
     if submitted at or before the deadline.
 -}
markSubmission :: Quiz -> Submission -> Double
markSubmission = error "TODO: implement marker"

{- `marker quizStr submissionsStr`
   combines the parsers and business logic as follows:

   If `quizStr` can be parsed to a quiz,
   and if `submissionsStr` can be parsed to
   a `[(String,Submission)]` using
   parseJSON and toResults,
   calculate quiz marks for each student
   and present them in the form of an
   sms update file.
   update files look like this:

     z1234567|quiz01|3
     z2345678|quiz01|7

   with each line being a | separated tuple
   of student ID, quiz name, and marks.
   No extra spaces beyond the newlines
   at the end of each line (including the
   last line).

   The order of the lines is not important.
 -}
marker :: String -> String -> Maybe String
marker = error "TODO: implement marker"

{- Use this to read a quiz key and submissions
   file from the file system, and print the
   updated file to stdOut.

   Note that FilePath is a type synonym for String.
 -}
runMarker :: FilePath -> FilePath -> IO ()
runMarker quizFile submissionsFile = do
  quiz <- readFile quizFile
  submissions <- readFile submissionsFile
  case marker quiz submissions of
    Nothing -> putStrLn "Something went wrong. But this error message is not helpful!"
    Just output -> putStrLn output
