data ComparableList = EmptyList | ComparableList Int ComparableList deriving (Eq)
instance Show ComparableList where
  show EmptyList = "[]_CL"
  show (ComparableList first others) = show(first) ++ " : " ++ show(others)
-- instance Eq ComparableList where
--   l1 == l2 = lengthCL l1 == lengthCL l2
instance Ord ComparableList where
  l1 <= l2 = lengthCL l1 <= lengthCL l2

list1 = EmptyList
list2 = ComparableList 4 EmptyList
list3 = ComparableList 2 list2
list4 = ComparableList 42 list3
list5 = ComparableList 1 (ComparableList 2 (ComparableList 3 EmptyList))
list6 = ComparableList 3 EmptyList

lengthCL :: ComparableList -> Int
lengthCL EmptyList                     = 0
lengthCL (ComparableList first others) = 1 + lengthCL others

mapCL :: (Int -> Int) -> ComparableList -> ComparableList
mapCL f EmptyList                     = EmptyList
mapCL f (ComparableList first others) = ComparableList (f first) (mapCL f others)