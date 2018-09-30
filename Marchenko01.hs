{-# OPTIONS_GHC -Wall #-}
module Marchenko01 where

-- Задача 1 -----------------------------------------
power3 :: [Integer]
power3 = [x*x*x | x <- [1 ..]]

-- Задача 2 -----------------------------------------
toPower3 :: [Integer]
toPower3 = [3^x | x <- [1 ..]]

-- Задача 3 -----------------------------------------
sumPower3 :: Integer -> Integer
sumPower3 i = sum [3^x | x <- [1 .. i]]

-- Задача 4 -----------------------------------------
sumPower :: Integer -> Integer -> Integer
sumPower m n = sum [m^i |(m >= 0), i <- [1, 2 .. n]]

-- Задача 5 -----------------------------------------
countLess :: [Int] -> Int -> Int
countLess [] n = 0
countLess (a:xs) n = if a < n then 1 + countLess xs n else countLess xs n

lessMe :: [Int] -> [Int]
lessMe xs = [countLess xs i | i <- xs]
-- lessMe xs = [y | x <- xs, let y = length [v | v <- xs, v<x]]
 
-- Задача 6 -----------------------------------------
frequency :: [Int] -> [(Int,Int)]
frequency [] = []
frequency (a:xs) = (a, (1 + length xs - length notA)):frequency notA where notA = [b | b <- xs, b /= a]

-- Задача 7 -----------------------------------------
hailstone :: Int -> Int
hailstone n = if (even n) then div n 2 else (n*3)+1 

-- Задача 8 -----------------------------------------
hailSeq :: Int -> [Int]
hailSeq n = if (n == 1) then [1] else n:hailSeq (hailstone n)

-- Задача 9 -----------------------------------------
allHailSeq :: [[Int]]
allHailSeq = [hailSeq x | x <- [1 ..]]

-- Задача 10 -----------------------------------------
firstHailSeq :: Int -> Int
firstHailSeq l = head [x | x <- [1 ..], (length (hailSeq x) == l)]
