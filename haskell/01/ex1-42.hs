-- ex1-42
-- 関数合成関数
-- ぶっちゃけ (.) と一緒
compose :: (b -> c) -> (a -> b) -> a -> c
compose fn gn x = fn (gn x)

main = do
    print $ compose (^2) (+1) 6
