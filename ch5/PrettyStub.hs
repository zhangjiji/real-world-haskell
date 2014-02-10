import SimpleJSON

data Doc = ToBeDefined
         deriving (Show)

string :: String -> Doc
string s = undefined

text :: String -> Doc
text t = undefined

double :: Double -> Doc
double d = undefined

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

fsep :: [Doc] -> Doc
fsep xs = undefined
