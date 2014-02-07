-- ch3/ch3.hs

-- data and type
data BookInfo = Book BookID BookName BookAuthors
              deriving (Show)
type BookID = Int
type BookName = String
type BookAuthors = [String]

-- enum
data Colors = Red
            | Green
            | Blue
            deriving (Eq, Show)

-- polymorphic
type Vector = (Double, Double)
data Shape = Circle Vector Double
           | Rectangle Vector Vector
           deriving (Show)

-- acc fun type
data Customer = Customer {
  customerID :: Int
  ,customerName :: String
  ,customerAddress :: String
   } deriving (Show)

-- recursive type define
data List a = Cons a (List a)
            | Nil
            deriving (Show)

data Tree a = Node a (Tree a) (Tree a)
            | EmptyNode
            deriving (Show, Eq)

fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

toList Nil = []
toList (Cons a t) = a:(toList t)

-- mimic java tree code
data MyTree a = TreeNode (Maybe a) (Maybe (MyTree a)) (Maybe (MyTree a))
              deriving (Show)


-- demo case of
data Fruit = Apple | Banana
           deriving (Show)
whichFruit :: String -> Fruit
whichFruit fruit = case fruit of
  "apple" -> Apple
  "banana" -> Banana
  
-- guard
nodeSame (Node a _ _) (Node b _ _)
  | a == b = Just a
nodeSame EmptyNode EmptyNode = Just EmptyNode
nodeSame _ _ = Nothing

lendMoney:: Double -> Double -> Maybe Double
lendMoney amount balance
  | amount <= 0 = Nothing
  | amount > reserve * 0.5 = Nothing
  | otherwise = Just newBalance
  where reserve = 100
        newBalance = balance - amount

niceDrop n xs
  | n <= 0 = xs
niceDrop _ [] = []
niceDrop n (_:xs) = niceDrop (n-1) xs

-- execise
length' :: (Num a) => [a] -> Int
length' [] = 0
length' (_:xs) = (length' xs) + 1

mean' [] = 0
mean' (xs) = (sum xs) / (fromIntegral (length xs))

palindrome [] = []
palindrome xs = xs ++ (reverse xs)

isPalindrome [] = True
isPalindrome xs
  | length xs == 1 = True
  | otherwise = (head xs == last xs) && (isPalindrome . init . tail $ xs)
