module FnRepeat(
    repeatN
)where

repeatN :: Int -> (a -> a) -> a -> a
repeatN 0 _  = id
repeatN n fn = fn . repeatN (n-1) fn

-- ex
main = do
    -- (*2)を3回適用する関数
    print $ repeatN 3 (*2) 5
