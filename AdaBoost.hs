module AdaBoost where

-- l == n
initializeNegativeWeights l n = take n repeat (1 / (2 * l))

-- m == p
initializePositiveWeights m p = take p repeat(1 / (2 * p))

normalizeWeights n p = map( / (sum $ n ++ p)) $ n ++ p

thresholdClassifier threshold, feature, polarity = if polarity * feature < threshold
                                                   then 1
                                                   else 0
