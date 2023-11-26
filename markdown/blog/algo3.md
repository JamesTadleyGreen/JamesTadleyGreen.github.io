# Sort

## Bubble Sort
This felt more difficult, I think due to how I think in Haskell the immutability
has really thrown me here. After an embarrassingly long time fighting here, I achieved
the below.
```haskell
bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = (iterate bubbleSort' xs) !! ((n * (n+1)) `div` 2)
    where n = length xs

bubbleSort' :: Ord a => [a] -> [a]
bubbleSort' [] = []
bubbleSort' [x] = [x]
bubbleSort' (x:y:xs)
    | x > y     = y : bubbleSort(x:xs)
    | otherwise = x : bubbleSort(y:xs)
```

### Notes
Wow this got hard fast, [this answer](https://codereview.stackexchange.com/questions/197868/bubble-sort-in-haskell)
shows that fold might have been the way to go, but to me it's less readable.

## Linked Lists
The point around insertion being \\(\mathcal{O}(1)\\) threw me a little, isn't it \\(\mathcal{O}(n)\\)
to traverse to the node that you wish to insert? This [answer](https://stackoverflow.com/questions/840648/why-is-inserting-in-the-middle-of-a-linked-list-o1)
shows the same idea. Tl;dr yes, but getting to the node is ignored.

## Queue
My first attempt didn't work, I could tell I was going down the wrong route.
My pseudo code is below, but I couldn't get the types to align.
```haskell
data Queue a = Queue {h :: Node a, t :: Node b}
data Node a = Node {value :: a, next :: Node a}

pop :: Queue a -> (a, Queue a)
pop (Queue {h=(Node {value=h', next=n}), t=t'}) = (h', Queue {h=n, t=t'})

peek :: Queue a -> a
peek (Queue {h=(Node {value=h', next=_}), t=_}) = h'

push :: Queue a -> a -> Queue a
push (Queue {h=h', t=t'}) a = (Queue {h=h', t=(Node {value=a, next=t'})})
```
I then looked up a [graph definition](https://stackoverflow.com/questions/9732084/how-do-you-represent-a-graph-in-haskell)
and I was closer than I thought. I was trying to generalise the types which didn't work well.

```haskell
import Data.List
import Data.Maybe

data Queue a =
  Queue
    { h :: Maybe (Node a)
    , t :: Maybe (Node a)
    }

data Node a =
  Node
    { value :: a
    , next :: Maybe (Node a)
    }

instance (Show a) => Show (Queue a) where
  show (Queue {h = h', t = t'}) =
    "<Queue | head: " ++ show h' ++ ", tail: " ++ show t' ++ ">"

instance (Show a) => Show (Node a) where
  show (Node {value = v, next = n}) = show v ++ show n

pop :: Queue a -> (Maybe a, Queue a)
pop q@(Queue {h = Nothing, t = _}) = (Nothing, q)
pop (Queue {h = Just (Node {value = h', next = n}), t = t'}) =
  (Just h', Queue {h = n, t = t'})

peek :: Queue a -> Maybe a
peek (Queue {h = Nothing, t = _}) = Nothing
peek (Queue {h = Just (Node {value = h', next = _}), t = _}) = Just h'

push :: Queue a -> a -> Queue a
push (Queue {h = Nothing, t = Nothing}) a = Queue {h = n, t = n}
  where
    n = Just (Node {value = a, next = Nothing})
push (Queue {h = h', t = t'}) a =
  (Queue {h = Just (Node {value = a, next = h'}), t = t' })
```

I think here the 'verboseness' of my solution shows ignorance on the language. I think
I should be able to construct this in 'simpler' terms and then 'lift' up to the
`Maybe` monad.

Additionally, the list implementation in Haskell is a linked list, therefore I can
define a `Queue` super simply as:
```haskell
newtype Queue a = Queue [a]

instance Show a => Show (Queue a) where
    show (Queue xs) = "Queue: " ++ show xs

push :: Queue a -> a -> Queue a
push (Queue xs) x = Queue $ xs ++ [x]

pop :: Queue a -> (Maybe a, Queue a)
pop (Queue []) = (Nothing, Queue [])
pop (Queue (x:xs)) = (Just x, Queue xs)

peak :: Queue a -> Maybe a
-- Reimplementing listToMaybe
peak (Queue []) = Nothing
peak (Queue (x:_)) = Just x
```
I think `(++)` is \\(\mathcal{O}(n)\\) due to having to traverse the list. There's probably a
better way to do this, but I'm not sure.

## Stack
Very similarly to the second implementation, of the queue:
```haskell
newtype Stack a = Stack [a]

instance Show a => Show (Stack a) where
    show (Stack xs) = "Stack: " ++ show xs

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> (Maybe a , Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)

peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x:_)) = Just x
```

This one 'feels' super \\(\mathcal{O}(1)\\) as we're only ever messing with the head. It suffers
from the same shortcomings as the queue.

In both of the above we didn;t bother defining `length`, it would be super easy to assign
this to the datastructure and carry this through our functions using `(+1)` and `(-1)`.

Additionally we could have defined functions like `safeHead` and then used `fmap` to
lift this. For example:

### RingBuffer
Below is my implementation of a ring buffer. I used arrays this time, to be a little
truer to the imperitive approach.
```haskell
import Data.Array

data RingBuffer e =
  RingBuffer
    { buffer :: Array Int e
    , head :: Int
    , tail :: Int
    }

instance Show e => Show (RingBuffer e) where
  show (RingBuffer b h t) =
    "RingBuffer (" ++ show b ++ ") " ++ show h ++ " " ++ show t

pop :: RingBuffer e -> (e, RingBuffer e)
pop (RingBuffer b h t) = (b ! t, RingBuffer b h (t + 1))

push :: RingBuffer e -> e -> RingBuffer e
push (RingBuffer b h t) e = RingBuffer (b // [(h + 1, e)]) (h + 1) t

peek :: RingBuffer e -> e
peek (RingBuffer b h _) = b ! h
```
<table class="table table-dark">
  <tr>
    <th>Data Structure</th>
    <th>Push</th>
    <th>Pop</th>
    <th>Peek</th>
    <th>Change at index</th>
  </tr>
  <tr>
    <td>Queue</td>
    <td>\\\(\mathcal{O}(1)\\\)</td>
    <td>\\\(\mathcal{O}(1)\\\)</td>
    <td>\\\(\mathcal{O}(1)\\\)</td>
    <td>\\\(\mathcal{O}(n)\\\)</td>
  </tr>
  <tr>
    <td>Stack</td>
    <td>\\\(\mathcal{O}(1)\\\)</td>
    <td>\\\(\mathcal{O}(1)\\\)</td>
    <td>\\\(\mathcal{O}(1)\\\)</td>
    <td>\\\(\mathcal{O}(n)\\\)</td>
  </tr>
  <tr>
    <td>Array</td>
    <td>\\\(\mathcal{O}(1)\\\)</td>
    <td>\\\(\mathcal{O}(1)\\\)</td>
    <td>\\\(\mathcal{O}(1)\\\)</td>
    <td>\\\(\mathcal{O}(1)\\\)</td>
  </tr>
  <tr>
    <td>RingBuffer</br>(ignoring up to resizing)</td>
    <td>\\\(\mathcal{O}(1)\\\)</td>
    <td>\\\(\mathcal{O}(1)\\\)</td>
    <td>\\\(\mathcal{O}(1)\\\)</td>
    <td>\\\(\mathcal{O}(1)\\\)</td>
  </tr>
</table>
