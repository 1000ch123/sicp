func :: Int -> Int
func 1 = 1
func 2 = 1
func 3 = 1
func x = func(x-1) + 2*func(x-2) + 3*func(x-3)

main = do
    print $ func 5
