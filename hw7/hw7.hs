module HW07 where

import System.Random

-- Exercise 1: Naive Fibonacci calculation
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2: Efficient Fibonacci calculation
fibs2 :: [Integer]
fibs2 = [0, 1] ++ zipWith (+) fibs2 (tail fibs2)

-- Exercise 3: Convert a stream to an infinite list
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x y) = x : streamToList y 

-- Exercise 4: Derive a Show instance for Stream
instance Show a => Show (Stream a) where
    show s = show $ take 20 $ streamToList s

-- Exercise 5: Functions for working with Streams
-- 5a) Create a stream as a repeating value
streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

-- 5b) Map a function over each element in a stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x y) = Cons (f x) $ streamMap f y

-- 5c) Create a stream by applying a function to the previous element
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f $ f x 

-- Exercise 6: Create a couple streams
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

testNats :: Bool
testNats = take 20 (streamToList nats) == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19]

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a s1) s2 = Cons a $ interleaveStreams s2 s1

ruler :: Stream Integer 
ruler = interleaveStreams s1 s2
    where s1 = streamRepeat 0
          s2 = streamMap (+ 1) nats

testRuler :: Bool
testRuler = take 15 (streamToList ruler) == [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0]

-- Exercise 7: Produce an infinite list of pseudo-random items
randomList :: (Random a, RandomGen g) => g -> [a]
randomList generator = x : randomList nextGenerator
    where (x, nextGenerator) = random generator 

-- Exercise 8: Create an infinite list of pseudo-random numbers
randomInts :: Int -> [Int]
randomInts x = randomList $ mkStdGen x

testRandomInts :: Bool
testRandomInts = ranList == [ran1, ran2]
    where ranList      = take 2 (randomInts 11)
          (ran1, gen1) = random $ mkStdGen 11
          (ran2, _)    = random gen1

-- Exercise 9: Profile the minMax function for a random list of 1,000,000 integers
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing   
minMax xs = Just (minimum xs, maximum xs)

main :: IO ()
main = case minMax $ take 1000000 $ randomInts 11 of
    Nothing     -> error "Cannot get minMax of randomInts"
    Just (x, y) -> print (x, y)

{-
 Memory profile:

(-9223371378522736183,9223364582230179031)
2,042,558,952 bytes allocated in the heap
  723,950,272 bytes copied during GC
  141,957,648 bytes maximum residency (11 sample(s))
          2,797,208 bytes maximum slop
                317 MB total memory in use (0 MB lost due to fragmentation)
-}
