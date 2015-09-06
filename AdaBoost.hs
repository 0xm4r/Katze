module AdaBoost where

import Data.Matrix

baseFeatureShapes =
    [fromLists [[1,0]],
    fromLists [[0],[1]],
    fromLists [[1,0,1]],
    fromLists [[1,0],[0,1]]]

type TData = [(Matrix Int, Int)]

data Feature = Int Int Int Int Int

integralImage :: Matrix Int -> Feature -> Int
integralImage m (Feature x y w h) = 
    where ii 0 _ = 0
          ii _ 0 = 0
          ii r c = ii (r-1) c + ii r (c-1) - ii (r-1) (c-1)

weakClassifier :: Int -> Int -> Feature -> (Matrix Int -> Int)
weakClassifier th p f = \m -> if p * integralImage f m < p * th then 1 else 0

initWeight :: Int -> Int -> Int -> Float
initWeight n p y = if y == 1
    then 1 / fromIntegral(2 * p)
    else 1 / fromIntegral(2 * n)

initWeights :: TData -> Int -> Int ->[Float]
initWeights ds n p = map (\x -> initWeight n p $ snd x) ds

chooseBestWeakClassifier :: TData -> [Float] -> [Int] -> (Matrix Int -> Int, Float, [Int])
chooseBestWeakClassifier td w fv =

computeNewWeights :: [Float] -> Float -> [Int] -> Float
computeNewWeights ow epsilon y = map(ow *) beta(epsilon)
