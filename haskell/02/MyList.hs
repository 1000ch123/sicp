module MyList(
    myLast,
    myReverse
)where

-- リストへの基本命令(cons/car/cdr)のみで，
-- リストへの各種操作を実装するぜ
-- Haskellで言うと (:),head,tail
-- case文のかわりにパターンマッチはokとする

-- | myListRef
-- >>> myListRef [0,1,2,3,4,5] 0
-- 0
-- >>> myListRef [0,1,2,3,4,5] 2
-- 2
myListRef :: [a] -> Int -> a
myListRef xs 0 = head xs
myListRef xs n = myListRef (tail xs) (n-1)

-- | myListRef
-- >>> myLength [0,1,2,3,4,5]
-- 6
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- | myAppend
-- >>> myAppend [1,2] [5,6]
-- [1,2,5,6]
myAppend :: [a] -> [a] -> [a]
myAppend [] ys     = ys
myAppend (x:xs) ys = x:myAppend xs ys

-- | myLast
-- >>> myLast [1,2,3,4]
-- 4
myLast :: [a] -> a
myLast [x]    = x
myLast (x:xs) = myLast xs


myReverse :: [a] -> [a]
myReverse (x:xs) = xs
