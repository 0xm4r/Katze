module AdaBoost where

normalize n p = map( / (sum $ n ++ p)) $ n ++ p
