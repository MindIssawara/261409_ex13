-- define instances of IfValue for as many types as you can
class IfValue a where
  boolVal :: a -> Bool

-- Instance for Int
instance IfValue Int where
  boolVal 0 = False
  boolVal _ = True

-- Instance for Integer
instance IfValue Integer where
  boolVal 0 = False
  boolVal _ = True

-- Instance for Bool 
instance IfValue Bool where
  boolVal x = x

-- Instance for Maybe
instance IfValue (Maybe a) where
  boolVal Nothing = False
  boolVal (Just _) = True

-- Instance for lists
instance IfValue [a] where
  boolVal [] = False
  boolVal _  = True

-- Instance for String
instance IfValue String where
  boolVal "" = False
  boolVal _  = True

-- Instance for floating-point numbers
instance IfValue Float where
  boolVal 0 = False
  boolVal _ = True

instance IfValue Double where
  boolVal 0 = False
  boolVal _ = True

-- define map for Maybe type
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing  = Nothing
mapMaybe f (Just x) = Just (f x)

-- define map for pairs, if you can
mapPair:: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)
