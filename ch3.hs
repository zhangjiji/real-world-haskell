-- ch3/ch3.hs

data BookInfo = Book BookID BookName BookAuthors
              deriving (Show)
type BookID = Int
type BookName = String
type BookAuthors = [String]

data Colors = Red
            | Green
            | Blue
            deriving (Eq, Show)

type Vector = (Double, Double)
data Shape = Circle Vector Double
           | Rectangle Vector Vector
           deriving (Show)

data Customer = Customer {
  customerID :: Int
  ,customerName :: String
  ,customerAddress :: String
   } deriving (Show)

data List a = Cons a (List a)
            | Nil
            deriving (Show)

data Tree a = Node a (Tree a) (Tree a)
            | EmptyNode
            deriving (Show)

fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

toList Nil = []
toList (Cons a t) = a:(toList t)
