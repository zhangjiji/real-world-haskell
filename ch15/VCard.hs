import Control.Monad

data Context = Home | Mobile | Business
             deriving (Eq, Show)

type Phone = String

albulena = [(Home, "+355-652-55512")]

nils = [(Mobile, "+47-922-55-512"), (Business, "+47-922-12-121"),
        (Home, "+47-925-55-121"), (Business, "+47-922-25-551")]

twalumba = [(Business, "+260-02-55-5121")]

onePersnalPhone :: [(Context, Phone)] -> Maybe Phone
onePersnalPhone alist =
  lookup Home alist `mplus` lookup Mobile alist

allBusinessPhones :: [(Context, Phone)] -> [Phone]
allBusinessPhones ps = map snd $ filter (contextIs Business) ps
                       `mplus` filter (contextIs Mobile) ps

contextIs a (b,_) = a == b

lookupM :: (MonadPlus m, Eq a) => a -> [(a,b)] -> m b
lookupM _ [] = mzero
lookupM key ((x,y):xys)
  | key == x = return y `mplus` lookupM key xys
  | otherwise = lookupM key xys

