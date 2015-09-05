module AdaBoost where

-- l == n
initializeNegativeWeights l n = take n repeat (1 / (2 * l))

-- m == p
initializePositiveWeights m p = take p repeat(1 / (2 * p))

normalizeWeights n p = map( / (sum $ n ++ p)) $ n ++ p

weakThresholdClassifier threshold feature polarity = if polarity * feature < threshold
                                                     then 1
                                                     else 0

computeError threshold feature polarity x y weight = sum(weight * abs $ weakThresholdClassifier threshold feature polarity - y)

strongThresholdClassifier alpha h = if sum(zipWith(*) alpha h)  > 0.5 * sum(alpha)
                                    then 1
                                    else 0
