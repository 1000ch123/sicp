-- ex2-15 理解促進のためのテスト
import Interval

one = makeInterval 100 100
interval1 = makeCenterPercent 100 1

main = do
    print $ percent $ one
    print $ percent $ interval1
    -- 数学的に one と等価だが,2回出ているので誤差が広く評価される
    print $ percent $ divInterval interval1 interval1
    -- 1回しか出ない分にはほとんど変わらない
    print $ percent $ divInterval  one (divInterval one interval1)
