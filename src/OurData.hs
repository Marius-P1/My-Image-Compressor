{-
-- EPITECH PROJECT, 2024
-- My-Image-Compressor
-- File description:
-- OurData
-}

module OurData (
        Color(..),
        Pixel(..),
    ) where

data Color = Color {
            red :: Int,
            green :: Int,
            blue :: Int
            } deriving (Eq)

data Pixel = Pixel {
            x :: Int,
            y :: Int,
            color :: Color
            } deriving (Eq)

instance Show Color where
    show (Color r g b) = "(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

instance Num Color where
    (+) (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 + r2) (g1 + g2) (b1 + b2)
    -- Les autres opérations ne sont pas définies
    (*) _ _ = error "Multiplication of colors is not defined"
    abs _ = error "abs is not defined for colors"
    signum _ = error "signum is not defined for colors"
    negate _ = error "negate is not defined for colors"
    fromInteger _ = error "fromInteger is not defined for colors"

instance Show Pixel where
    show (Pixel valX valY valColor) =
        "(" ++ show valX ++ "," ++ show valY ++ ") " ++ show valColor
