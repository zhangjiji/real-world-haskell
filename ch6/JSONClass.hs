{-# LANGUAGE TypeSynonymInstances #-}

module JSONClass (
  JSON(..)
  )where

import Control.Arrow (second)

data JValue = JString String
  |JNumber Double
  |JBool Bool
  |JNull
  |JObject (JObj JValue) --[(String, JValue)]
  |JArray (JAry JValue) --[JValue]
   deriving (Eq, Ord, Show)

type JSONError = String

class JSON a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
  toJValue = id
  fromJValue = Right

instance JSON Bool where
  toJValue = JBool
  fromJValue (JBool b) = Right b
  fromJValue _ = Left "not a JSON boolean"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "not a JSON number"

instance JSON Int where
  toJValue = JNumber . realToFrac
  fromJValue = doubleToJValue round

instance JSON Integer where
  toJValue = JNumber . realToFrac
  fromJValue = doubleToJValue round

instance JSON Double where
  toJValue = JNumber
  fromJValue = doubleToJValue id

newtype JAry a = JAry {
  fromJAry :: [a]
  } deriving (Eq, Ord, Show)

newtype JObj a = JObj {
  fromJObj :: [(String, a)]
  } deriving (Eq, Ord, Show)

jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jaryFromJValue (JArray (JAry a)) = whenRight JAry (mapEithers fromJValue a)
jaryFromJValue _ = Left "not a JSON Array"

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left a) = Left a
whenRight f (Right b) = Right (f b)

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
  Left b -> Left b
  Right ys -> case f x of
    Left err -> Left err
    Right y -> Right (y:ys)
mapEithers _ _ = Right []

jaryToJValue :: (JSON a) => (JAry a) -> JValue
jaryToJValue = jaryOfJValueToJValue . jvaluesToJAry . listToJValues . fromJAry

instance (JSON a) => JSON (JAry a) where
  toJValue = jaryToJValue
  fromJValue = jaryFromJValue

listToJValues :: (JSON a) => [a] -> [JValue]
listToJValues = map toJValue

jvaluesToJAry :: [JValue] -> JAry JValue
jvaluesToJAry = JAry

jaryOfJValueToJValue :: JAry JValue -> JValue
jaryOfJValueToJValue = JArray

instance (JSON a) => JSON (JObj a) where
  toJValue = JObject . JObj . map (second toJValue) . fromJObj
  fromJValue (JObject (JObj a)) = whenRight JObj (mapEithers unwrap a)
    where unwrap (k,v) = whenRight ((,) k) (fromJValue v)
  fromJValue _ = Left "not JSON Object"


-- instance JSON String where
--   toJValue = JString
--   fromJValue (JString s) = Right s
--   fromJValue _ = Left "not string"

-- instance (JSON a) => JSON [a] where
--   toJValue = JString
--   fromJValue (JString s) = Right s
--   fromJValue _ = Left "not a JSON String"

