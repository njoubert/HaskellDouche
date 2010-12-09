--- Functions & Pattern Matching

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe x = "You're a big one"

sayDo :: (Integral a) => a -> String
sayDo x = case x of
	1 -> "Doe!"
	2 -> "Rei!"
	3 -> "Me!"
	4 -> "Fa!"
	5 -> "So!"
	6 -> "La!"
	7 -> "Ti!"
	8 -> "Doeeeee!"
	_ -> "Boinkkkk"

--- Type Definitions

data Point = Point Float Float deriving (Show)

data Shape = 
	Circle Point Float | 
	Rectangle Point Point
	deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs (x2 - x1)) * abs( (y2 - y1))

data Person = Person {
	firstName :: String,
	lastName :: String,
	age :: Int,
	height :: Float,
	phoneNumber :: String,
	flavor :: String
} deriving (Show)

data MyMaybe a = MyNothing | MyJust a ---"a" is a type type parameter

data MyRational = MyRational {numerator :: Integer, denominator :: Integer}
createRational :: Integer -> Integer -> MyRational
createRational n d = if (signum d) < 0 then MyRational (-1 * n) (-1 * d) else MyRational n d

--- Type Classes

class YesNo a where
	yesno :: a -> Bool

--- Type Class Instance

instance Eq Point where
	x == y = 
		case x of 
			(Point f1a f1b) -> case y of 
				(Point f2a f2b) -> (f1a == f2a) && (f1b == f2b)

instance Eq Shape where
	(Circle p r) == (Circle p2 r2) = (p == p2) && (r == r2)
	(Rectangle p1a p1b) == (Rectangle p2a p2b) = (p1a == p2a) && (p1b == p2b)
	_ == _ = False

instance Eq Person where
	(Person fn1 ln1 a1 h1 p1 f1) == (Person fn2 ln2 a2 h2 p2 f2) =
		(fn1 == fn2) &&
		(ln2 == ln2) &&
		(a1 == a2) &&
		(h1 == h2) &&
		(p1 == p2) &&
		(f1 == f2)

instance (Eq m) => Eq (MyMaybe m) where --- instance of type constructor, not concrete type. Introduce type variable and add constaint on type variable.
	MyJust x == MyJust y = x == y
	MyNothing == MyNothing = True
	_ == _ = False

instance Eq MyRational where
	(MyRational n1 d1) == (MyRational n2 d2) = (n1 == n2) && (d1 == d2)

instance Show MyRational where
	show (MyRational n d) = show n ++ "/" ++ show d

instance Num MyRational where
	(MyRational n1 d1) + (MyRational n2 d2) = MyRational (n1*d2 + n2*d1) (d1*d2)
	(MyRational n1 d1) * (MyRational n2 d2) = MyRational (n1*n2) (d1*d2)
	abs (MyRational n d) = MyRational (abs n) (abs d)
	signum (MyRational n d) = if (signum d) > -1 then (MyRational (signum n) 1) else (MyRational (0 - (signum n)) 1)
	fromInteger i = MyRational i 1

--- Cannot do this because the type checker cannot decide what type "b" can be... :-(
---instance (Num b) => YesNo b where
---	yesno x = if ((signum x) == 0) then False else True

instance YesNo Int where
	yesno 0 = False
	yesno _ = True

instance YesNo [a] where
	yesno [] = False
	yesno _ = True

instance YesNo Bool where
	yesno = id
	
instance YesNo (Maybe a) where
	yesno (Just _) = True
	yesno Nothing = False
	


--- functions
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

---
main = do {
	print (sayMe 3 ++ "; " ++ sayMe 10 ++ "; " ++ sayDo 3 ++ "; " ++ sayDo 10);
	print c;
	print $ surface c;
	print $ surface r;
	print $ r == r;
	print $ r == c;
	print $ p1 == p1;
	print $ p1 == p2;
	print $ n1 + n2;
	print $ n1 * n2;
	print $ yesnoIf (Just 500) "Yea!" "No!";
	print $ yesnoIf Nothing "Yea!" "No!";
	print $ yesnoIf (1 :: Int) "Yea!" "No!";
	print $ yesnoIf (0 :: Int) "Yea!" "No!";
} where {
	c = Circle (Point 10 20) 10;
	r = Rectangle (Point 0 0) (Point 10 10);
	p1 = Person {firstName="Niels", lastName="Joubert", age=24, height=75.0, phoneNumber="6508231662", flavor="Coffeebeans"};
	p2 = (Person "Dieter" "Joubert" 21 75.0 "3433" "Sand");
	n1 = MyRational 3 5;
	n2 = MyRational 2 7;
}