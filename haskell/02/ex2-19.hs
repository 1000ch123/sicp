-- ex2-19
-- コイン両替プログラム改良
-- 利用可能コインをリストで与えるパターン

-- もともとこんなかんじで回答していた
countExchangePatterns :: Int -> [Int] -> Int
countExchangePatterns _ []  = 0
countExchangePatterns n (x:xs)
    | n == 0    = 1
    | n < 0     = 0
    | otherwise = countExchangePatterns n xs + countExchangePatterns (n-x) (x:xs)

-- やるべきは
-- firstDenomination:car
-- exceptFirstDenomination:cdr
-- noMore:null?
-- だけじゃね？
