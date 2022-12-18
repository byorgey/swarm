{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Swarm.Util
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A random collection of small, useful functions that are (or could
-- be) used throughout the code base.
module Swarm.Util (
  -- * Miscellaneous utilities
  (?),
  maxOn,
  maximum0,
  cycleEnum,
  listEnums,
  uniq,
  getElemsInArea,
  manhattan,
  binTuples,

  -- * Directory utilities
  readFileMay,
  readFileMayT,
  getSwarmDataPath,
  getSwarmSavePath,
  getSwarmHistoryPath,
  readAppData,

  -- * Text utilities
  isIdentChar,
  replaceLast,

  -- * English language utilities
  reflow,
  quote,
  squote,
  commaList,
  indefinite,
  indefiniteQ,
  singularSubjectVerb,
  plural,
  number,

  -- * Validation utilities
  holdsOr,
  isJustOr,
  isRightOr,
  isSuccessOr,

  -- * Template Haskell utilities
  liftText,

  -- * Lens utilities
  (%%=),
  (<%=),
  (<+=),
  (<<.=),
  (<>=),
  _NonEmpty,

  -- * Utilities for NP-hard approximation
  smallHittingSet,
  getDataDirSafe,
  getDataFileNameSafe,
  dataNotFound,
) where

import Control.Algebra (Has)
import Control.Effect.State (State, modify, state)
import Control.Effect.Throw (Throw, throwError)
import Control.Exception (catch)
import Control.Exception.Base (IOException)
import Control.Lens (ASetter', Lens', LensLike, LensLike', Over, lens, (<>~))
import Control.Lens.Lens ((&))
import Control.Monad (forM, unless, when)
import Data.Bifunctor (first)
import Data.Char (isAlphaNum)
import Data.Either.Validation
import Data.Int (Int32)
import Data.List (maximumBy, partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text, toUpper)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Tuple (swap)
import Data.Yaml
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)
import NLP.Minimorph.English qualified as MM
import NLP.Minimorph.Util ((<+>))
import Paths_swarm (getDataDir)
import Swarm.Util.Location
import System.Clock (TimeSpec)
import System.Directory (
  XdgDirectory (XdgData),
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesFileExist,
  getXdgDirectory,
  listDirectory,
 )
import System.FilePath
import System.IO
import System.IO.Error (catchIOError)
import Witch

-- $setup
-- >>> import qualified Data.Map as M
-- >>> import Swarm.Util.Location

infixr 1 ?
infix 4 %%=, <+=, <%=, <<.=, <>=

-- | A convenient infix flipped version of 'fromMaybe': @Just a ? b =
--   a@, and @Nothing ? b = b@. It can also be chained, as in @x ? y ?
--   z ? def@, which takes the value inside the first @Just@,
--   defaulting to @def@ as a last resort.
(?) :: Maybe a -> a -> a
(?) = flip fromMaybe

-- | Find the maximum of two values, comparing them according to a
--   custom projection function.
maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn f x y
  | f x > f y = x
  | otherwise = y

-- | Find the maximum of a list of numbers, defaulting to 0 if the
--   list is empty.
maximum0 :: (Num a, Ord a) => [a] -> a
maximum0 [] = 0
maximum0 xs = maximum xs

-- | Take the successor of an 'Enum' type, wrapping around when it
--   reaches the end.
cycleEnum :: (Eq e, Enum e, Bounded e) => e -> e
cycleEnum e
  | e == maxBound = minBound
  | otherwise = succ e

listEnums :: (Enum e, Bounded e) => [e]
listEnums = [minBound .. maxBound]

-- | Drop repeated elements that are adjacent to each other.
--
-- >>> uniq []
-- []
-- >>> uniq [1..5]
-- [1,2,3,4,5]
-- >>> uniq (replicate 10 'a')
-- "a"
-- >>> uniq "abbbccd"
-- "abcd"
uniq :: Eq a => [a] -> [a]
uniq = \case
  [] -> []
  (x : xs) -> x : uniq (dropWhile (== x) xs)

-- | Manhattan distance between world locations.
manhattan :: Location -> Location -> Int32
manhattan (Location x1 y1) (Location x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- | Get elements that are in manhattan distance from location.
--
-- >>> v2s i = [(p, manhattan origin p) | x <- [-i..i], y <- [-i..i], let p = Location x y]
-- >>> v2s 0
-- [(P (V2 0 0),0)]
-- >>> map (\i -> length (getElemsInArea origin i (M.fromList $ v2s i))) [0..8]
-- [1,5,13,25,41,61,85,113,145]
--
-- The last test is the sequence "Centered square numbers":
-- https://oeis.org/A001844
getElemsInArea :: Location -> Int32 -> Map Location e -> [e]
getElemsInArea o@(Location x y) d m = M.elems sm'
 where
  -- to be more efficient we basically split on first coordinate
  -- (which is logarithmic) and then we have to linearly filter
  -- the second coordinate to get a square - this is how it looks:
  --         ▲▲▲▲
  --         ││││    the arrows mark points that are greater then A
  --         ││s│                                 and lesser then B
  --         │sssB (2,1)
  --         ssoss   <-- o=(x=0,y=0) with d=2
  -- (-2,-1) Asss│
  --          │s││   the point o and all s are in manhattan
  --          ││││                  distance 2 from point o
  --          ▼▼▼▼
  sm =
    m
      & M.split (Location (x - d) (y - 1)) -- A
      & snd -- A<
      & M.split (Location (x + d) (y + 1)) -- B
      & fst -- B>
  sm' = M.filterWithKey (const . (<= d) . manhattan o) sm

-- | Place the second element of the tuples into bins by
-- the value of the first element.
binTuples ::
  (Foldable t, Ord a) =>
  t (a, b) ->
  Map a (NE.NonEmpty b)
binTuples = foldr f mempty
 where
  f = uncurry (M.insertWith (<>)) . fmap pure

------------------------------------------------------------
-- Directory stuff

-- | Safely attempt to read a file.
readFileMay :: FilePath -> IO (Maybe String)
readFileMay = catchIO . readFile

-- | Safely attempt to (efficiently) read a file.
readFileMayT :: FilePath -> IO (Maybe Text)
readFileMayT = catchIO . T.readFile

-- | Turns any IO error into Nothing.
catchIO :: IO a -> IO (Maybe a)
catchIO act = (Just <$> act) `catchIOError` (\_ -> return Nothing)

getDataDirSafe :: FilePath -> IO (Maybe FilePath)
getDataDirSafe p = do
  d <- mySubdir <$> getDataDir
  de <- doesDirectoryExist d
  if de
    then return $ Just d
    else do
      xd <- mySubdir . (</> "data") <$> getSwarmDataPath False
      xde <- doesDirectoryExist xd
      return $ if xde then Just xd else Nothing
 where
  mySubdir d = d `appDir` p
  appDir r = \case
    "" -> r
    "." -> r
    d -> r </> d

getDataFileNameSafe :: FilePath -> IO (Maybe FilePath)
getDataFileNameSafe name = do
  dir <- getDataDirSafe "."
  case dir of
    Nothing -> return Nothing
    Just d -> do
      let fp = d </> name
      fe <- doesFileExist fp
      return $ if fe then Just fp else Nothing

dataNotFound :: FilePath -> IO Text
dataNotFound f = do
  d <- getSwarmDataPath False
  let squotes = squote . T.pack
  return $
    T.unlines
      [ "Could not find the data: " <> squotes f
      , "Try downloading the Swarm 'data' directory to: " <> squotes d
      ]

-- | Get path to swarm data, optionally creating necessary
--   directories. This could fail if user has bad permissions
--   on his own $HOME or $XDG_DATA_HOME which is unlikely.
getSwarmDataPath :: Bool -> IO FilePath
getSwarmDataPath createDirs = do
  swarmData <- getXdgDirectory XdgData "swarm"
  when createDirs (createDirectoryIfMissing True swarmData)
  pure swarmData

-- | Get path to swarm saves, optionally creating necessary
--   directories.
getSwarmSavePath :: Bool -> IO FilePath
getSwarmSavePath createDirs = do
  (</> "saves") <$> getSwarmDataPath createDirs

-- | Get path to swarm history, optionally creating necessary
--   directories.
getSwarmHistoryPath :: Bool -> IO FilePath
getSwarmHistoryPath createDirs =
  (</> "history") <$> getSwarmDataPath createDirs

-- | Read all the .txt files in the data/ directory.
readAppData :: IO (Map Text Text)
readAppData = do
  md <- getDataDirSafe "."
  case md of
    Nothing -> fail . T.unpack =<< dataNotFound "<the data directory itself>"
    Just d -> do
      fs <-
        filter ((== ".txt") . takeExtension)
          <$> ( listDirectory d `catch` \e ->
                  hPutStr stderr (show (e :: IOException)) >> return []
              )
      M.fromList . mapMaybe sequenceA
        <$> forM fs (\f -> (into @Text (dropExtension f),) <$> readFileMayT (d </> f))

------------------------------------------------------------
-- Some Text-y stuff

-- | Predicate to test for characters which can be part of a valid
--   identifier: alphanumeric, underscore, or single quote.
--
-- >>> isIdentChar 'A' && isIdentChar 'b' && isIdentChar '9'
-- True
-- >>> isIdentChar '_' && isIdentChar '\''
-- True
-- >>> isIdentChar '$' || isIdentChar '.' || isIdentChar ' '
-- False
isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''

-- | @replaceLast r t@ replaces the last word of @t@ with @r@.
--
-- >>> :set -XOverloadedStrings
-- >>> replaceLast "foo" "bar baz quux"
-- "bar baz foo"
-- >>> replaceLast "move" "(make"
-- "(move"
replaceLast :: Text -> Text -> Text
replaceLast r t = T.append (T.dropWhileEnd isIdentChar t) r

------------------------------------------------------------
-- Some language-y stuff

-- | Reflow text by removing newlines and condensing whitespace.
reflow :: Text -> Text
reflow = T.unwords . T.words

-- | Prepend a noun with the proper indefinite article (\"a\" or \"an\").
indefinite :: Text -> Text
indefinite w = MM.indefiniteDet w <+> w

-- | Prepend a noun with the proper indefinite article, and surround
--   the noun in single quotes.
indefiniteQ :: Text -> Text
indefiniteQ w = MM.indefiniteDet w <+> squote w

-- | Combine the subject word with the simple present tense of the verb.
--
-- Only some irregular verbs are handled, but it should be enough
-- to scrap some error message boilerplate and have fun!
--
-- >>> :set -XOverloadedStrings
-- >>> singularSubjectVerb "I" "be"
-- "I am"
-- >>> singularSubjectVerb "he" "can"
-- "he can"
-- >>> singularSubjectVerb "The target robot" "do"
-- "The target robot does"
singularSubjectVerb :: Text -> Text -> Text
singularSubjectVerb sub verb
  | verb == "be" = case toUpper sub of
      "I" -> "I am"
      "YOU" -> sub <+> "are"
      _ -> sub <+> "is"
  | otherwise = sub <+> (if is3rdPerson then verb3rd else verb)
 where
  is3rdPerson = toUpper sub `notElem` ["I", "YOU"]
  verb3rd
    | verb == "have" = "has"
    | verb == "can" = "can"
    | otherwise = fst $ MM.defaultVerbStuff verb

-- | Pluralize a noun.
plural :: Text -> Text
plural = MM.defaultNounPlural

-- For now, it is just MM.defaultNounPlural, which only uses heuristics;
-- in the future, if we discover specific nouns that it gets wrong,
-- we can add a lookup table.

-- | Either pluralize a noun or not, depending on the value of the
--   number.
number :: Int -> Text -> Text
number 1 = id
number _ = plural

-- | Surround some text in single quotes.
squote :: Text -> Text
squote t = T.concat ["'", t, "'"]

-- | Surround some text in double quotes.
quote :: Text -> Text
quote t = T.concat ["\"", t, "\""]

-- | Make a list of things with commas and the word "and".
commaList :: [Text] -> Text
commaList [] = ""
commaList [t] = t
commaList [s, t] = T.unwords [s, "and", t]
commaList ts = T.unwords $ map (`T.append` ",") (init ts) ++ ["and", last ts]

------------------------------------------------------------
-- Some orphan instances

deriving instance FromJSON TimeSpec
deriving instance ToJSON TimeSpec

------------------------------------------------------------
-- Validation utilities

-- | Require that a Boolean value is @True@, or throw an exception.
holdsOr :: Has (Throw e) sig m => Bool -> e -> m ()
holdsOr b e = unless b $ throwError e

-- | Require that a 'Maybe' value is 'Just', or throw an exception.
isJustOr :: Has (Throw e) sig m => Maybe a -> e -> m a
Just a `isJustOr` _ = return a
Nothing `isJustOr` e = throwError e

-- | Require that an 'Either' value is 'Right', or throw an exception
--   based on the value in the 'Left'.
isRightOr :: Has (Throw e) sig m => Either b a -> (b -> e) -> m a
Right a `isRightOr` _ = return a
Left b `isRightOr` f = throwError (f b)

-- | Require that a 'Validation' value is 'Success', or throw an exception
--   based on the value in the 'Failure'.
isSuccessOr :: Has (Throw e) sig m => Validation b a -> (b -> e) -> m a
Success a `isSuccessOr` _ = return a
Failure b `isSuccessOr` f = throwError (f b)

------------------------------------------------------------
-- Template Haskell utilities

-- See https://stackoverflow.com/questions/38143464/cant-find-inerface-file-declaration-for-variable
liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

------------------------------------------------------------
-- Fused-Effects Lens utilities

(<+=) :: (Has (State s) sig m, Num a) => LensLike' ((,) a) s a -> a -> m a
l <+= a = l <%= (+ a)
{-# INLINE (<+=) #-}

(<%=) :: (Has (State s) sig m) => LensLike' ((,) a) s a -> (a -> a) -> m a
l <%= f = l %%= (\b -> (b, b)) . f
{-# INLINE (<%=) #-}

(%%=) :: (Has (State s) sig m) => Over p ((,) r) s s a b -> p a (r, b) -> m r
l %%= f = state (swap . l f)
{-# INLINE (%%=) #-}

(<<.=) :: (Has (State s) sig m) => LensLike ((,) a) s s a b -> b -> m a
l <<.= b = l %%= (,b)
{-# INLINE (<<.=) #-}

(<>=) :: (Has (State s) sig m, Semigroup a) => ASetter' s a -> a -> m ()
l <>= a = modify (l <>~ a)
{-# INLINE (<>=) #-}

------------------------------------------------------------
-- Other lens utilities

_NonEmpty :: Lens' (NonEmpty a) (a, [a])
_NonEmpty = lens (\(x :| xs) -> (x, xs)) (const (uncurry (:|)))

------------------------------------------------------------
-- Some utilities for NP-hard approximation

-- | Given a list of /nonempty/ sets, find a hitting set, that is, a
--   set which has at least one element in common with each set in the
--   list.  It is not guaranteed to be the /smallest possible/ such
--   set, because that is NP-hard.  Instead, we use a greedy algorithm
--   that will give us a reasonably small hitting set: first, choose
--   all elements in singleton sets, since those must necessarily be
--   chosen.  Now take any sets which are still not hit, and find an
--   element which occurs in the largest possible number of remaining
--   sets. Add this element to the set of chosen elements, and filter
--   out all the sets it hits.  Repeat, choosing a new element to hit
--   the largest number of unhit sets at each step, until all sets are
--   hit.  This algorithm produces a hitting set which might be larger
--   than optimal by a factor of lg(m), where m is the number of sets
--   in the input.
--
-- >>> import qualified Data.Set as S
-- >>> shs = smallHittingSet . map S.fromList
--
-- >>> shs ["a"]
-- fromList "a"
--
-- >>> shs ["ab", "b"]
-- fromList "b"
--
-- >>> shs ["ab", "bc"]
-- fromList "b"
--
-- >>> shs ["acd", "c", "aef", "a"]
-- fromList "ac"
--
-- >>> shs ["abc", "abd", "acd", "bcd"]
-- fromList "cd"
--
-- Here is an example of an input for which @smallHittingSet@ does
-- /not/ produce a minimal hitting set. "bc" is also a hitting set and
-- is smaller.  b, c, and d all occur in exactly two sets, but d is
-- unluckily chosen first, leaving "be" and "ac" unhit and
-- necessitating choosing one more element from each.
--
-- >>> shs ["bd", "be", "ac", "cd"]
-- fromList "cde"
smallHittingSet :: Ord a => [Set a] -> Set a
smallHittingSet ss = go fixed (filter (S.null . S.intersection fixed) choices)
 where
  (fixed, choices) = first S.unions . partition ((== 1) . S.size) . filter (not . S.null) $ ss

  go !soFar [] = soFar
  go !soFar cs = go (S.insert best soFar) (filter (not . (best `S.member`)) cs)
   where
    best = mostCommon cs

  -- Given a nonempty collection of sets, find an element which is shared among
  -- as many of them as possible.
  mostCommon :: Ord a => [Set a] -> a
  mostCommon = fst . maximumBy (comparing snd) . M.assocs . M.fromListWith (+) . map (,1 :: Int) . concatMap S.toList
