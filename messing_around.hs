data Tree a = Leaf a | Node (Tree a) (Tree a)

a = (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3))

-- Show for Trees:
-- TODO: rewrite this to be an actual show function on the type

myshow (Leaf x) = print x
myshow (Node x y) = do { myshow x; myshow y }

-- Map over Trees:

maptree f (Leaf x) = (Leaf (f x))
maptree f (Node x y) = (Node (maptree f x) (maptree f y))

-- Reduce over Trees:

reduce f (Leaf x) = x
reduce f (Node x y) = (f (reduce f x) (reduce f y))

-- Currying higher order functions:

mycurry f = \a -> \b -> f (a,b)
myuncurry f = \(a,b) -> f a b

uncsub (x,y) = x - y

-- Infinite data structures

evens = [x*2 | x <- [1..]]
odds = [x*2+1 | x <- [0..]]

merge [x] [] = [x]
merge [] [y] = [y]
merge (xf:xr) (yf:yr) = if xf <= yf then xf:yf:(merge xr yr) else yf:xf:(merge xr yr)