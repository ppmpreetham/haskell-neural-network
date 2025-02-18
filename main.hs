module Main where

import Prelude

-- Activation Functions

relu :: Double -> Double -- RELU
relu x
    | x >= 0 = x
    | otherwise = 0

sigmoid :: Double -> Double -- SIGMOID
sigmoid x = 1/(1 + exp(-x))

linear:: Double -> Double; linear x = x --LINEAR
-- Layer Functions

-- Dense
dense :: (Double -> Double) -> [Double] -> [Double] -> [Double] -> [Double]
dense activation xs ws bs = map (\(x, (w, b)) -> activation $ w * x + b) $ zip xs $ zip ws bs

-- Sequential
-- Each layer is represented as a tuple (weights, biases)
Sequential :: (Double -> Double) -> [Double] -> [([Double], [Double])] -> [Double]
Sequential activation input layers = foldl (\acc (ws, bs) -> dense activation acc ws bs) input layers

-- Driver
-- First dense layer
x :: [Double]
x = [1,2,3,4,5]

w1 :: [Double]
w1 = [2,4,6,8,10]

b1 :: [Double]
b1 = [-1,-2,-3,-4,-5]

-- Second dense layer
w2 :: [Double]
w2 = [1,1,1,1,1]

b2 :: [Double]
b2 = [0,0,0,0,0]

main :: IO ()
main = do
    putStrLn "Dense layer with ReLU activation:"
    print $ dense relu x w1 b1
    putStrLn "Sequential network with two layers (ReLU activation):"
    print $ Sequential relu x [(w1, b1), (w2, b2)]