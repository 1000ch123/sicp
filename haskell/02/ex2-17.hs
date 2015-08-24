-- ex2-17
-- Listへの命令(cons/car/cdr)のみで lastを実装せよ
-- 引数は空でないリストを想定して良い
import MyList

main = do
    print $ myLast [1,2,3,4]
