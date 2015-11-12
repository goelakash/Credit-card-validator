module Validate where

toDigits :: Integer -> [Integer]
toDig 0 = [0]
toDig x = toDig (x`div`10) ++ [x`mod`10]

toDigits xs = tail (toDig xs)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)


doubleSecond :: [Integer] -> [Integer]
doubleSecond = \ xs ->
	map (\(a,b) -> if odd b then a*2 else a) (zip xs [0..])


sumDigits :: [Integer] -> Integer
sumDigits xs = 
	sum [sum(toDigits(d)) | d <- xs]

isValid :: Integer -> Bool
isValid x
	| sumDigits(doubleSecond(toDigitsRev x)) `mod` 10 == 0 = True
	| otherwise = False 
