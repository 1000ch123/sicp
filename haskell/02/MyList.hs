module MyList(
    myLast,
)where

-- | myLast
-- >>> myLast [1,2,3,4]
-- 4
myLast :: [a] -> a
myLast [x]    = x
myLast (x:xs) = myLast xs


