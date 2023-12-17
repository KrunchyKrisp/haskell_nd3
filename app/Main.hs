module Main where

main :: IO ()
main = do
	putStrLn "Hello, Haskell!"

-- 1
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
	deriving (Show, Ord, Eq)

overlaps :: Shape -> Shape -> Bool
overlaps (Circle x1 y1 r1) (Circle x2 y2 r2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2 < (r1 + r2) ^ 2
overlaps (Rectangle x1 y1 a1 b1) (Rectangle x2 y2 a2 b2) =
	x1 + a1 >= x2
		&& x1 <= x2 + a2
		&& y1 + b1 >= y2
		&& y1 <= y2 + b2
overlaps (Rectangle x2 y2 a2 b2) (Circle x1 y1 r1) = overlaps (Circle x1 y1 r1) (Rectangle x2 y2 a2 b2) -- see below
overlaps (Circle x1 y1 r1) (Rectangle x2 y2 a2 b2) = (x1 - closestX) ^ 2 + (y1 - closestY) ^ 2 < r1 ^ 2
	where
		-- closest (x,y) of the rectangle to center of the circle
		closestX = min (x2 + a2) (max x1 x2)
		closestY = min (y2 + b2) (max y1 y2)

-- 2
any1, any2 :: (a -> Bool) -> [a] -> Bool
any1 f xs = length (filter f xs) > 0
any2 f xs = foldr (||) False (map f xs)

all1, all2 :: (a -> Bool) -> [a] -> Bool
all1 f xs = length (filter f xs) == length xs
all2 f xs = foldr (&&) True (map f xs)

-- 3
unzip1 :: [(a, b)] -> ([a], [b])
unzip1 xs = foldr f ([], []) xs
	where
		f (x1, y1) (xs, ys) = (x1 : xs, y1 : ys)

-- 4
length1, length2 :: [a] -> Int
length1 xs = (sum . map (const 1)) xs
length2 xs = foldr (\_ y -> y + 1) 0 xs

-- 5
ff :: Integer -> [Integer] -> Integer
ff x = last . filter (<= x) . scanl (+) 0 . map (10 *) . filter (>= 0)

-- 6
total1, total2 :: (Integer -> Integer) -> Integer -> Integer
total1 f n = (sum . map f) [0 .. n]
total2 f n = foldr ((+) . f) 0 [0 .. n]

-- 7
iter1, iter2 :: Int -> (a -> a) -> (a -> a)
iter1 n f
	| n <= 0 = id
	| otherwise = iter1 (n - 1) f . f
iter2 n f
	| n <= 0 = id
	| otherwise = foldr (.) f (replicate (n - 1) f)

-- 8
splits :: [a] -> [([a], [a])]
splits xs = zip left right
	where
		split = map (: []) xs
		left = scanl (++) [] split
		right = scanr (++) [] split
