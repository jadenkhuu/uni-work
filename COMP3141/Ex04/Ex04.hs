module Ex04 where

import Data.Semigroup
import Data.Monoid
import Control.Monad.State (State, get, put, evalState)
import Test.QuickCheck
import Priority
import Size

-- DEFINITIONS AND HELPER FUNCTIONS --

type NodeInfo = (Size, Priority)
data QueueTree a
  = Null
  | Leaf NodeInfo a
  | Node NodeInfo (QueueTree a) (QueueTree a)
  deriving (Show)

nodeInfo :: QueueTree a -> NodeInfo
nodeInfo Null = mempty
nodeInfo (Leaf i _) = i
nodeInfo (Node i _ _) = i

sizeOf :: QueueTree a -> Size
sizeOf = fst . nodeInfo

maxPrio :: QueueTree a -> Priority
maxPrio = snd . nodeInfo

-- checks whether the tree structure
-- is balanced (i.e. that the left subtree and the right
-- subtree don't ever differ too much in size)
balanced :: QueueTree a -> Bool
balanced (Node i l r) =
  let sl = unSize (sizeOf l) in
  let sr = unSize (sizeOf r) in
  abs (sl - sr) <= 1 && balanced l && balanced r
balanced _ = True



-- EXERCISE STARTS HERE --

-- Task 1a. Write a well-formedness predicate
--          wf for the `QueueTree` data type.

-- Hint: Both `Priority` and `Size` are semigroups/monoids.
-- This means that the type `NodeInfo` is also automatically
-- a monoid.

wf :: QueueTree a -> Bool
wf Null = True
wf (Leaf (x,y) a) = (unSize x == 1) && (unPriority y >= 0)
wf (Node i l r) =  (i == nodeInfo l <> nodeInfo r) && wf l && wf r

-- Task 1b. Write smart constructors `leaf` and `node`
--          for the `QueueTree` data type which maintain
--          the well-formedness invariant. I.e. given
--          well-formed inputs, the smart constructors
--          should give well-formed outputs.
--          You should /not/ tweak the structure of the ~QueueTree~
--          beyond updating the ~NodeInfo~; in particular don't do
--          ~node Null Null = Null~.

leaf :: Priority -> a -> QueueTree a
leaf p a = Leaf ((size 1),p) a

node :: QueueTree a -> QueueTree a -> QueueTree a
node l r = Node (nodeInfo l <> nodeInfo r) l r

-- node l r = Node (size(unSize(sizeOf l) + unSize(sizeOfr)), priority(max unPriority(maxPrio l) unPriority(maxPrio r))) l r
-- use monad to make a node and use the node as an argument



-- Task 2a. Implement the usual priority queue functions
--          for the type `QueueTree`. These are
--          pop - Remove the element from the queue that has the
--               highest priority. Return the modified queue,
--               along with the removed element (if any).
--          insert - add an element to the queue with the given priority.

pop :: QueueTree a -> (QueueTree a, Maybe a)
pop Null = (Null, Nothing)
pop (Leaf i a) = (Null, Just a)
pop (Node i l r)
  | maxPrio l == (snd i) = 
    let (x, a) = pop l 
    in (node x r, a)
  | maxPrio r == (snd i) =
    let (y, a) = pop r
    in (node l y, a)

insert :: Priority -> a -> QueueTree a -> QueueTree a
insert p a Null = leaf p a
insert p a (Leaf i x) = node (leaf p a) (Leaf i x)
insert p a (Node i l r) = 
  if sizeL <= sizeR 
  then
    node (insert p a l) r 
  else
    node l (insert p a r)
  where
    sizeL = sizeOf l
    sizeR = sizeOf r

-- Task 2b. Implement a function `fromList` that converts a
--          list of `(Priority, x)` pairs into a well-formed
--          and balanced `QueueTree x` structure.

fromList :: [(Priority, a)] -> QueueTree a
fromList x = foldr (uncurry insert) Null x

-- Hint: you can use `fromList` to implement an `Arbitrary`
-- instance for `QueueTree`, allowing you to test your work.


-- Task 3. Implement stateful versions of the pop and insert
--         operations above using the `State` type in Haskell's
--         standard mtl library.
--         Implement a `peek` operation which just returns the
--         highest-priority element without changing the
--         state of the queue.
--         Do not use the `state` function in your final
--         implementations!

pop' :: State (QueueTree a) (Maybe a)
pop' = get >>= \t ->
    let 
      (t', a) = pop t
    in 
      put t' >> 
      return a

insert' :: Priority -> a -> State (QueueTree a) ()
insert' p a = get >>= \t ->
  let 
    t' = insert p a t 
  in 
    put t'

peek' :: State (QueueTree a) (Maybe a)
peek' = do 
  t <- get
  let (t', a) = pop t
  return a



-- END OF EXERCISE --

-- You can use the following three examples to test your
-- implementations of pop' and insert', and to practice
-- reading `State`-ful functions.

-- Returns the highest priority currently in the `QueueTree`
-- without changing the state.
getMaxPrio' :: State (QueueTree a) Priority
getMaxPrio' =
  get >>= \q ->
  return (maxPrio q)

-- Removes the element with the second-highest priority
-- in the queue.
dip' :: State (QueueTree a) ()
dip' =
  getMaxPrio' >>= \p ->
  pop'        >>= \h1 ->
  pop'        >>= \h2 ->
  case h1 of
    Nothing -> return ()
    Just h1 -> insert' p h1

-- a `State`-free version of dip
dip :: QueueTree Char -> QueueTree Char
dip = evalState $
  dip' >>= \() ->
  get
