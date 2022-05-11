-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE MagicHash #-}

module Util (IsBool (..), group, ungroup, pad, padLeft, delIdx, replaceIdx,
             insertIdx, mvIdx, mapFst, mapSnd, splitOn, scan, scanM, composeN,
             slice, sliceText, mapMaybe, uncons, repeated,
             transitiveClosure, transitiveClosureM,
             showErr, listDiff, splitMap, enumerate, iota, restructure,
             onSnd, onFst, findReplace, swapAt, uncurry3,
             measureSeconds, sameConstructor,
             bindM2, foldMapM, lookupWithIdx, (...), zipWithT, for, getAlternative,
             Zippable (..), zipWithZ_, zipErr, forMZipped, forMZipped_,
             whenM, unsnoc, anyM,
             File (..), FileHash, FileContents, addHash, readFileWithHash,
             SpanTree (..), SpanTreeM (..), SpanPayload, SpanPos,
             evalSpanTree, makeSpanTree, makeEmptySpanTree, makeSpanTreeRec, fillTreeAndAddTrivialLeaves) where

import Crypto.Hash
import Data.Functor.Identity (Identity(..))
import Data.Char (isSpace)
import Data.List (sort, findIndex)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString    as BS
import Data.Foldable
import Data.Maybe (listToMaybe, maybeToList)
import qualified Data.Text as T
import Err
import Prelude
import qualified Data.Set as Set
import qualified Data.Map.Strict as M
import Control.Applicative
import Control.Monad.State.Strict
import System.CPUTime
import GHC.Base (getTag)
import GHC.Exts ((==#), tagToEnum#)
import Debug.Trace

class IsBool a where
  toBool :: a -> Bool

iota :: (Enum a, Integral a) => a -> [a]
iota n = take (fromEnum n) [0..] -- XXX: `[0..(n-1)]` is incorrect for unsigned ints!

swapAt :: Int -> a -> [a] -> [a]
swapAt _ _ [] = error "swapping to empty list"
swapAt 0 y (_:xs) = y:xs
swapAt n y (x:xs) = x:(swapAt (n-1) y xs)

onFst :: (a -> b) -> (a, c) -> (b, c)
onFst f (x, y) = (f x, y)

onSnd :: (a -> b) -> (c, a) -> (c, b)
onSnd f (x, y) = (x, f y)

unsnoc :: NonEmpty a -> ([a], a)
unsnoc (x:|xs) = case reverse (x:xs) of
  (y:ys) -> (reverse ys, y)
  _ -> error "impossible"

enumerate :: Traversable f => f a -> f (Int, a)
enumerate xs = evalState (traverse addCount xs) 0
  where addCount :: a -> State Int (Int, a)
        addCount x = do n <- get
                        put (n + 1)
                        return (n, x)

splitMap :: Ord k => [k] -> M.Map k v -> (M.Map k v, M.Map k v)
splitMap ks m = let ks' = Set.fromList ks
                    pos = M.filterWithKey (\k _ -> k `Set.member` ks') m
                in (pos, m M.\\ pos)

listDiff :: Ord a => [a] -> [a] -> [a]
listDiff xs ys = Set.toList $ Set.difference (Set.fromList xs) (Set.fromList ys)

showErr :: Show e => Either e a -> Either String a
showErr (Left e) = Left (show e)
showErr (Right x) = Right x

group :: (Ord a) => [(a,b)] -> [(a, [b])]
group [] = []
group ((k,v):xs) =
  let (prefix, suffix) = span ((== k) . fst) xs
      g = v:(map snd prefix)
  in (k, g) : group suffix

ungroup ::  [(a, [b])] -> [(a,b)]
ungroup [] = []
ungroup ((k,vs):xs) = (zip (repeat k) vs) ++ ungroup xs

uncons :: [a] -> (a, [a])
uncons (x:xs) = (x, xs)
uncons [] = error "whoops! [uncons]"

pad :: a -> Int -> [a] -> [a]
pad v n xs = xs ++ replicate (n - length(xs)) v

padLeft :: a -> Int -> [a] -> [a]
padLeft v n xs = replicate (n - length(xs)) v ++ xs

delIdx :: Int -> [a] -> [a]
delIdx i xs = case splitAt i xs of
  (prefix, _:suffix) -> prefix ++ suffix
  (prefix, []) -> prefix -- Already not there

replaceIdx :: Int -> a -> [a] -> [a]
replaceIdx i x xs = case splitAt i xs of
  (prefix, _:suffix) -> prefix ++ (x:suffix)
  (prefix, []) -> prefix ++ [x]

insertIdx :: Int -> a -> [a] -> [a]
insertIdx i x xs = case splitAt i xs of
  (prefix, suffix) -> prefix ++ (x:suffix)

mvIdx :: Int -> Int -> [a] -> [a]
mvIdx i j xs | j == i = xs
             | j < i = let x = xs!!i
                       in insertIdx j x . delIdx i $ xs
             | otherwise = let x = xs!!i
                           in  delIdx i . insertIdx j x $ xs

mapFst :: (a -> b) -> [(a, c)] -> [(b, c)]
mapFst f zs = [(f x, y) | (x, y) <- zs]

mapSnd :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnd f zs = [(x, f y) | (x, y) <- zs]

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = let rest = mapMaybe f xs
                    in case f x of
                        Just y  -> y : rest
                        Nothing -> rest

composeN :: Int -> (a -> a) -> a -> a
composeN n f = foldr (.) id (replicate n f)

repeated :: Ord a => [a] -> [a]
repeated = repeatedSorted . sort

repeatedSorted :: Eq a => [a] -> [a]
repeatedSorted [] = []
repeatedSorted [_] = []
repeatedSorted (x:y:rest) | x == y = [x] ++ repeatedSorted (dropWhile (== x) rest)
                          | otherwise = repeatedSorted (y:rest)

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn f s = let (prefix, suffix) = break f s
              in case suffix of
                   [] -> [prefix]
                   _:xs -> prefix : splitOn f xs

restructure :: Traversable f => [a] -> f b -> f a
restructure xs structure = evalState (traverse procLeaf structure) xs
  where procLeaf :: b -> State [a] a
        procLeaf _ = do ~(x:rest) <- get
                        put rest
                        return x

-- TODO: find a more efficient implementation
findReplace :: Eq a => [a] -> [a] -> [a] -> [a]
findReplace _ _ [] = []
findReplace old new s@(x:xs) =
  if take n s == old
    then new ++ recur (drop n s)
    else x : recur xs
  where n = length old
        recur = findReplace old new

scan :: Traversable t => (a -> s -> (b, s)) -> t a -> s -> (t b, s)
scan f xs s = runState (traverse (asState . f) xs) s

scanM :: (Monad m, Traversable t) => (a -> s -> m (b, s)) -> t a -> s -> m (t b, s)
scanM f xs s = runStateT (traverse (asStateT . f) xs) s

asStateT :: Monad m => (s -> m (a, s)) -> StateT s m a
asStateT f = do
  s <- get
  (ans, s') <- lift $ f s
  put s'
  return ans

asState :: (s -> (a, s)) -> State s a
asState f = asStateT (Identity . f)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f ma mb = do
  a <- ma
  b <- mb
  f a b

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f ... g = \x y -> f $ g x y

foldMapM :: (Monad m, Monoid w, Foldable t) => (a -> m w) -> t a -> m w
foldMapM f xs = foldM (\acc x -> (acc<>) <$> f x ) mempty xs

lookupWithIdx :: Eq a => a -> [(a, b)] -> Maybe (Int, b)
lookupWithIdx k vals = lookup k $ [(x, (i, y)) | (i, (x, y)) <- zip [0..] vals]

-- NOTE: (toList args) has to be at least as long as (toList trav)
zipWithT :: (Traversable t, Monad h, Foldable f) => (a -> b -> h c) -> t a -> f b -> h (t c)
zipWithT f trav args = flip evalStateT (toList args) $ flip traverse trav $ \e -> getNext >>= lift . f e
  where getNext = get >>= \(h:t) -> put t >> return h

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

transitiveClosure :: forall a. Ord a => (a -> [a]) -> [a] -> [a]
transitiveClosure getParents seeds =
  toList $ execState (mapM_ go seeds) mempty
  where
    go :: a -> State (Set.Set a) ()
    go x = do
      visited <- get
      unless (x `Set.member` visited) $ do
        modify $ Set.insert x
        mapM_ go $ getParents x

transitiveClosureM :: forall m a. (Monad m, Ord a) => (a -> m [a]) -> [a] -> m [a]
transitiveClosureM getParents seeds =
  toList <$> execStateT (mapM_ go seeds) mempty
  where
    go :: a -> StateT (Set.Set a) m ()
    go x = do
      visited <- get
      unless (x `Set.member` visited) $ do
        modify (<> Set.singleton x)
        lift (getParents x) >>= mapM_ go

measureSeconds :: MonadIO m => m a -> m (a, Double)
measureSeconds m = do
  t1 <- liftIO $ getCPUTime
  ans <- m
  t2 <- liftIO $ getCPUTime
  return (ans, (fromIntegral $ t2 - t1) / 1e12)

whenM :: Monad m => m Bool -> m () -> m ()
whenM test doit = test >>= \case
  True -> doit
  False -> return ()

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM f xs = do
  conds <- mapM f xs
  return $ any id conds

-- === zippable class ===

class Zippable f where
  zipWithZ :: MonadFail m => (a -> b -> m c) -> f a -> f b -> m (f c)

instance Zippable [] where
  zipWithZ _ [] [] = return []
  zipWithZ f (x:xs) (y:ys) = (:) <$> f x y <*> zipWithZ f xs ys
  zipWithZ _ _ _ = zipErr

instance Zippable NE.NonEmpty where
  zipWithZ f xs ys = NE.fromList <$> zipWithZ f (NE.toList xs) (NE.toList ys)

zipWithZ_ :: Zippable f => MonadFail m => (a -> b -> m c) -> f a -> f b -> m ()
zipWithZ_ f xs ys = zipWithZ f xs ys >> return ()

zipErr :: MonadFail m => m a
zipErr = fail "zip error"

forMZipped :: Zippable f => MonadFail m => f a -> f b -> (a -> b -> m c) -> m (f c)
forMZipped xs ys f = zipWithZ f xs ys

forMZipped_ :: Zippable f => MonadFail m => f a -> f b -> (a -> b -> m c) -> m ()
forMZipped_ xs ys f = void $ forMZipped xs ys f

getAlternative :: Alternative m => [a] -> m a
getAlternative xs = asum $ map pure xs
{-# INLINE getAlternative #-}

-- === bytestrings paired with their hash digest ===

-- TODO: use something other than a string to store the digest
type FileHash     = String
type FileContents = BS.ByteString

-- TODO: consider adding mtime as well for a fast path that doesn't
-- require reading the file
data File = File
  { fContents :: FileContents
  , fHash     :: FileHash }
  deriving (Show, Eq, Ord)

addHash :: FileContents -> File
addHash s = File s $ show (hash s :: Digest SHA256)

readFileWithHash :: MonadIO m => FilePath -> m File
readFileWithHash path = liftIO $ addHash <$> BS.readFile path

sameConstructor :: a -> a -> Bool
sameConstructor x y = tagToEnum# (getTag x ==# getTag y)
{-# INLINE sameConstructor #-}

-- TODO(danielzheng): Use SpanId instead of Int
-- data SpanTree a = Span {
--   spanId :: Int,
--   spanChildren :: [SpanTree a]
-- } | Leaf {
--   spanLeafId :: Int,
--   spanLeafData :: a
-- } | Trivia {
--   spanTrivia :: a
-- }

type SpanPayload = (Int, Int, SpanId)
type SpanPos = (Int, Int)

data SpanTree a = Span SpanPayload [SpanTree a] | Leaf SpanPayload a | Trivia SpanPos a
  deriving (Show, Eq)

-- Indexing?
-- Go from character offset to innermost span

-- flattenSpanTree

-- String ->

-- Idea: often having indexing issue, not sure if offsets are inclusive or
-- exclusive. Cool to have type of position and type of position transitions.
-- The hope is Dex index sets deal with fencepost errors.
--
-- Index type and "index posts" (inclusive, exclusive, refers to gaps).
--
-- We have fences (content) and posts (delimiters), and status quo is ints used
-- for both. We have function from fence to post and post to previous fence.
--
--   [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
-- a: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- b: ~~~~~~~~~~~~~~~~
-- c: ~~~~~~~
-- d:          ~~~~~~~
-- e:                   ~~~~~~~~~~

-- State on SpanPayloads + Maybe
newtype SpanTreeM a = SpanTreeM
  { runSpanTree' :: StateT [SpanPayload] Maybe a }
  deriving (Functor, Applicative, Monad, MonadState [SpanPayload], Alternative)

evalSpanTree :: SpanTreeM a -> [SpanPayload] -> Maybe a
evalSpanTree m = evalStateT $ runSpanTree' m

getNextSpanPayload :: SpanTreeM (Maybe SpanPayload)
getNextSpanPayload = SpanTreeM $ do
  infos <- get
  case infos of
    [] -> return Nothing
    x:xs -> put xs >> return (Just x)

makeSpanTreeRec' :: SpanPayload -> SpanTreeM [SpanTree ()]
makeSpanTreeRec' current = do
  -- do { infos <- get; traceM $ "state: " ++ show infos ++ ", current: " ++ show current}
  getNextSpanPayload >>= \case
    Just child ->
      -- trace ("contained? " ++ show (contained current child))
      case contained current child of
        Contained -> do
          childTree <- makeSpanTreeRec child
          -- traceM $ "childTree: " ++ show childTree
          remainingChildren <- makeSpanTreeRec' current
          -- traceM $ "remainingChildren: " ++ show remainingChildren
          return (childTree : remainingChildren)
        NotContained -> do { infos <- get; put (child : infos) >> return [] }
        -- Overlap -> empty -- throws error
        -- Overlap -> return []
        Overlap -> do { infos <- get; put (child : infos) >> return [] }
        -- Overlap -> error "Noooo"
    Nothing -> return []

makeSpanTreeRec :: SpanPayload -> SpanTreeM (SpanTree ())
makeSpanTreeRec current = do
  children <- makeSpanTreeRec' current
  case children of
    [] -> return (Leaf current ())
    _ -> return (Span current children)

makeEmptySpanTree :: [SpanPayload] -> Maybe (SpanTree ())
makeEmptySpanTree [] = Nothing
-- makeEmptySpanTree (x:xs) = evalSpanTree (makeSpanTreeRec x) xs
makeEmptySpanTree (x:xs) =
  let tree = evalSpanTree (makeSpanTreeRec x) xs in
  trace ("makeEmptySpanTree: " ++ show tree) tree

-- makeSpanTree :: [a] -> [SpanPayload] -> Maybe (SpanTree [a])
makeSpanTree :: (Show a, IsTrivia a) => [a] -> [SpanPayload] -> Maybe (SpanTree ())
makeSpanTree xs infos = case makeEmptySpanTree infos of
  Nothing -> Nothing
  Just posTree -> Just (fillTreeAndAddTrivialLeaves xs posTree)

data SpanContained = Contained | NotContained | Overlap
  deriving (Show, Eq)

contained :: SpanPayload -> SpanPayload -> SpanContained
contained (lpos, rpos, _) (lpos', rpos', _) =
  -- case (compare lpos lpos', compare rpos rpos', compare rpos, lpos') of
  case (lpos <= lpos', rpos >= rpos') of
    (True, True) -> Contained
    (False, False) -> NotContained
    _ -> if rpos <= lpos'
      then NotContained
      else Overlap

-- slice :: Int -> Int -> [a] -> [a]
-- slice left right xs = take (right - left) (drop left xs)

slice :: Int -> Int -> [a] -> [a]
slice left right xs = take (right - left) (drop left xs)

sliceText :: Int -> Int -> T.Text -> T.Text
sliceText left right xs = T.take (right - left) (T.drop left xs)

getSpanPos :: SpanTree a -> SpanPos
getSpanPos tree = case tree of
  Span (l, r, _) _ -> (l, r)
  Leaf (l, r, _) _ -> (l, r)
  Trivia pos _ -> pos

-- blah :: [(Int, Int)] -> [SpanTree ()]
-- fillTrivia :: SpanPayload -> [SpanTree a] -> [SpanTree a]
fillTrivia :: SpanPayload -> [SpanTree ()] -> [SpanTree ()]
fillTrivia (l, r, _) offsets =
  -- TODO: Add trivia before and after
  let (before, after) = case offsets of
                [] -> ([], [])
                _ ->
                  let (headL, _) = getSpanPos (head offsets) in
                  let (_, tailR) = getSpanPos (last offsets) in
                  let before' = [Trivia (l, headL) () | l /= headL] in
                  let after' = [Trivia (tailR, r) () | r /= tailR] in
                  (before', after') in
  let offsets' = before ++ offsets ++ after in
  -- let offsets' = trace ("before: " ++ show before ++ ", after: " ++ show after) offsets in
  -- let pairs = zip offsets (drop 1 offsets) in
  -- let pairs = trace (show offsets') zip offsets' (drop 1 offsets') in
  let pairs = zip offsets' (drop 1 offsets') in
  -- let gaps = map (\(p@(_, r), p'@(l, _)) -> l - r) pairs in
  let unzipped = pairs >>= getOffsetAndTrivia in
  maybeToList (listToMaybe offsets') ++ unzipped
  where getOffsetAndTrivia :: (SpanTree (), SpanTree ()) -> [SpanTree ()]
        getOffsetAndTrivia (t, t') =
          let (_, r') = endpoints t in
          let (l', _) = endpoints t' in
          let diff = l' - r' in
          if diff == 0 then
            [t']
          else
            [Trivia (r', l') (), t']

rebalanceTrivia :: Show a => (a -> Bool) -> [a] -> [SpanTree ()] -> [SpanTree ()]
rebalanceTrivia space xs trees =
  -- trees
  -- FIXME(danielzheng): This duplicates a ton of content for some reason.
  let whitespaceSeparated = trees >>= createTrivia in
  whitespaceSeparated
  -- let triviaMerged = mergeTrivia whitespaceSeparated in
  -- triviaMerged
  where
    createTrivia :: SpanTree () -> [SpanTree ()]
    createTrivia t = case t of
      Span _ _ -> [t]
      Leaf _ _ -> blah
      Trivia _ _ -> blah
      where blah :: [SpanTree ()]
            blah =
              let (l, r) = endpoints t in
              let s' = slice l r xs in
              let firstNonspace = findIndex (not . space) s' in
              let lastNonspace = fmap (length s' -) (findIndex (not . space) (reverse s')) in
              -- trace (show t ++ " " ++ show s' ++ "\nfirstNonspace: " ++ show firstNonspace ++ ", lastNonspace: " ++ show lastNonspace)
              case (firstNonspace, lastNonspace) of
                (Just l', Nothing) | l' > 0 -> [Trivia (l, l + l') (), shiftTree (l + l', r) t]
                (Nothing, Just r') | r' < length s' -> [shiftTree (l, l + r') t, Trivia (l + r', r) ()]
                (Just l', Just r') | l' > 0 || r' < length s' ->
                  -- trace "boom\n"
                  [Trivia (l, l + l') (), shiftTree (l + l', l + r') t, Trivia (l + r', r) ()]
                (_, _) -> [t]

    shiftTree :: SpanPos -> SpanTree () -> SpanTree ()
    shiftTree (l', r') t = case t of
      Span (_, _, i) children -> Span (l', r', i) children
      Leaf (_, _, i) x -> Leaf (l', r', i) x
      Trivia _ x -> Trivia (l', r') x

    mergeTrivia :: [SpanTree ()] -> [SpanTree ()]
    mergeTrivia trees' = reverse $ foldl' f [] trees'
      where f :: [SpanTree ()] -> SpanTree () -> [SpanTree ()]
            f acc y = case acc of
              [] -> [y]
              x:rest -> case (x, y) of
                (Trivia (l, _) _, Trivia (_, r') _) -> Trivia (l, r') () : rest
                (_, _) -> y:acc

{-
    mergeTrivia' :: (SpanTree (), SpanTree ()) -> [SpanTree ()]
    mergeTrivia' (t, t') = case (t, t') of
      (t0@(Trivia _ _), t1@(Trivia _ _)) -> []
      _ -> [t]
-}

endpoints :: SpanTree a -> (Int, Int)
endpoints (Span (l, r, _) _) = (l, r)
endpoints (Leaf (l, r, _) _) = (l, r)
endpoints (Trivia (l, r) _) = (l, r)

class IsTrivia a where
  isTrivia :: a -> Bool

instance IsTrivia Char where
  isTrivia = isSpace

fillTreeAndAddTrivialLeaves :: Show a => IsTrivia a => [a] -> SpanTree () -> SpanTree ()
fillTreeAndAddTrivialLeaves xs tree = case tree of
  Span info children ->
    let children' = fillTrivia info children in
    let children'' = rebalanceTrivia isTrivia xs children' in
    -- let children' = fillTrivia info ([Trivia (l, l) undefined] ++ children ++ [Trivia (r, r) undefined]) in
    let filled = map (fillTreeAndAddTrivialLeaves xs) children'' in
    Span info filled
  Leaf (l, r, spanId) _ -> Leaf (l, r, spanId) ()
  Trivia (l, r) _ -> Trivia (l, r) ()

{-
fillTreeAndAddTrivialLeaves :: [a] -> SpanTree b -> SpanTree [a]
fillTreeAndAddTrivialLeaves xs tree = case tree of
  Span info children ->
    -- let leaves = filter filterLeaf children in
    {-
    let offsets = map endpoints children in
    let pairs = zip offsets (drop 1 offsets) in
    let gaps = map (\((_, r), (l, _)) -> l - r) pairs in
    let mask = map (== 0) gaps in
    let final = mapIfElse mask in
    let filled = map (fillTreeAndAddTrivialLeaves xs) children in
    -- TODO: Make trivial here
    Span info filled
    -}
    -- TODO: Make trivial around outermost parent so that trivia is created around
    -- TODO: Robust: check parent l and r versus children's – current approach breaks syntax highlighting
    let children' = fillTrivia info children in
    -- let children' = fillTrivia info ([Trivia (l, l) undefined] ++ children ++ [Trivia (r, r) undefined]) in
    let filled = map (fillTreeAndAddTrivialLeaves xs) children' in
    Span info filled
  Leaf (l, r, spanId) _ -> Leaf (l, r, spanId) (slice l (r + 1) xs)
  Trivia (l, r) _ -> Trivia (l, r) (slice l (r + 1) xs)
-}
  {-
  where filterLeaf :: SpanTree a -> Bool
        filterLeaf (Leaf _ _) = True
        filterLeaf _ = False

        -- I don't like bool's argument order
        if_ :: Bool -> a -> a -> a
        if_ True  a _ = a
        if_ False _ b = b

        mapIfElse :: [Bool] -> [a] -> [a] -> [a]
        mapIfElse = zipWith3 if_
  -}

-- High-level: this inverts a tree traversal
{-
makeSpanTree :: [a] -> [SpanPayload] -> Maybe (SpanTree [a])
makeSpanTree xs infos =
  -- Sort infos': increasing left, decreasing right.
  let infos' = sort infos in
  -- 1st is root.
  -- 2nd is first child.
  -- 3rd is either: first child of 2nd, or second child of root.
  let childrenInfos = filter (filterFn parent) infos' in
  error ""
  where
    filterFn :: SpanPayload -> SpanPayload -> Bool
    filterFn parent info = isContained (contained parent info)

  -- Recursive function: takes current span, infos, returns span tree and list of what's left
  -- - If nothing left
-}

{-

:p meanAndStdDev numSamps estimatePiAvgVal (new_key 0)

SrcPosCtx {srcPosCtxSrcPos = Just (3,53), srcPosCtxExprId = Just 0}
SrcPosCtx {srcPosCtxSrcPos = Just (3,43), srcPosCtxExprId = Just 1}
SrcPosCtx {srcPosCtxSrcPos = Just (3,26), srcPosCtxExprId = Just 2}
SrcPosCtx {srcPosCtxSrcPos = Just (3,17), srcPosCtxExprId = Just 3}
SrcPosCtx {srcPosCtxSrcPos = Just (17,26), srcPosCtxExprId = Just 4}
SrcPosCtx {srcPosCtxSrcPos = Just (26,43), srcPosCtxExprId = Just 5}
SrcPosCtx {srcPosCtxSrcPos = Just (44,53), srcPosCtxExprId = Just 6}
SrcPosCtx {srcPosCtxSrcPos = Just (44,52), srcPosCtxExprId = Just 7}
SrcPosCtx {srcPosCtxSrcPos = Just (52,53), srcPosCtxExprId = Just 8}

:p
<span id="span-0">
meanAndStdDev numSamps estimatePiAvgVal
<span>
(
<span>
new_key
</span> 0)
</span>

-}
