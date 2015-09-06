module AdaBoost where

import Data.Matrix

--1 : white 0: black
baseFeatureShapes =
    [fromLists [[1,0]],
    fromLists [[0],[1]],
    fromLists [[1,0,1]],
    fromLists [[1,0],[0,1]]]

type TData = [(Matrix Int, Int)]

--shape row col width height
data Feature = Feature Int Int Int Int Int

safeGetElem 0 _ _ = 0
safeGetElem _ 0 _ = 0
safeGetElem r c s = getElem r c s

integralImage :: Matrix Int -> Matrix Int
integralImage m = ii (fromList (nrows m) (ncols m) [0..]) 1 1
    where fillCell s r c = setElem (safeGetElem (r-1) c s + safeGetElem r (c-1) s
            - safeGetElem (r-1) (c-1) s + safeGetElem r c m) (r, c) s
          ii s r c
              |r < nrows s = ii (fillCell s r c) (r + 1) c
              |c < ncols s = ii (fillCell s r c) 1 (c + 1)
              |otherwise = fillCell s r c

--Matrix is integralimage
getFeatureScore :: Matrix Int -> Feature -> Int
getFeatureScore s (Feature sh row col width height)
    |sh == 0 = 0 - getSquare row col width height + getSquare row (col + width) width height
    |sh == 1 = getSquare row col width height - getSquare (row + height) col width height
    |sh == 2 = 0 - getSquare row col width height 
        + getSquare row (col + width) width height
        - getSquare row (col + 2 * width) width height
    |sh == 3 = 0 - getSquare row col width height + getSquare row (col + width) width height
        + getSquare (row + height) col width height - getSquare (row + height) (col + width) width height
    where getSquare r c w h = safeGetElem (r' + h) (c' + w) s
                              - safeGetElem (r' + h) c' s
                              - safeGetElem r' (c' + w) s
                              + safeGetElem r' c' s
              where r' = r - 1
                    c' = c - 1

weakClassifier :: Int -> Int -> Feature -> Matrix Int -> (Matrix Int -> Int)
weakClassifier th p f intImg = \m -> if p * getFeatureScore intImg f < p * th then 1 else 0

initWeight :: Int -> Int -> Int -> Float
initWeight n p y = if y == 1
    then 1 / fromIntegral(2 * p)
    else 1 / fromIntegral(2 * n)

initWeights :: TData -> Int -> Int ->[Float]
initWeights ds n p = map (\x -> initWeight n p $ snd x) ds

-- chooseBestWeakClassifier :: TData -> [Float] -> [Int] -> (Matrix Int -> Int, Float, [Int])
-- chooseBestWeakClassifier td w fv =

-- computeNewWeights :: [Float] -> Float -> [Int] -> Float
-- computeNewWeights ow epsilon y = map(ow *) beta(epsilon)
-- n=10
-- intImg = integralImage $ fromList n n [1..n*n]

-- main = do 
--     print $ getFeatureScore intImg (Feature 2 3 3 2 2)
--     print $ fromList n n [1..n*n]