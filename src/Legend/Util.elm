module Legend.Util exposing (indexedMap2)

import List


indexedMap2 : (Int -> a -> b -> c) -> List a -> List b -> List c
indexedMap2 f xs ys =
    List.map3 f
        (List.range 0 (List.length xs - 1))
        xs
        ys
