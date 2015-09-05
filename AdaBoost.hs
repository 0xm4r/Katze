module AdaBoost where

import Data.Matrix

type TData = [(Matrix Int, Int)]

data WeakH = WeakH Float Int Feature

data Feature = Int Int Int Int Int

initWeight :: Int -> Int -> Int -> Float
initWeight n p y = if y == 1
                   then 1 / fromIntegral(2 * p)
                   else 1 / fromIntegral(2 * n)

initWeights :: TData -> Int -> Int ->[Float]
initWeights ds n p = map (\x -> initWeight n p $ snd x) ds
