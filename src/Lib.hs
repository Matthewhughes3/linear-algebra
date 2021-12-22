{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import Control.Applicative
import GHC.Exts

newtype Vector a = Vector {unVectorize :: [a]}
  deriving (Functor, Foldable, Semigroup, Monoid, Eq, Ord)

instance Show a => Show (Vector a) where
  show (Vector xs) = show xs

instance IsList (Vector a) where
  type Item (Vector a) = a
  fromList = Vector
  toList = unVectorize

mkMatrix :: [[a]] -> Matrix a
mkMatrix = Vector . fmap Vector

vCons :: a -> Vector a -> Vector a
vCons x (Vector xs) = Vector (x : xs)

zapp :: [a -> b] -> [a] -> [b]
zapp [] _ = []
zapp _ [] = []
zapp (f : fs) (x : xs) = f x : zapp fs xs

instance Applicative Vector where
  pure = Vector . repeat
  (Vector fs) <*> (Vector xs) = Vector $ zapp fs xs

instance Traversable Vector where
  sequenceA = foldr (liftA2 vCons) (pure mempty)

instance Num a => Num (Vector a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  (-) = liftA2 (-)

type Matrix a = Vector (Vector a)

vectorMatrixMultiply :: Num a => Matrix a -> Vector a -> Vector a
vectorMatrixMultiply x = sum . (* x) . fmap pure

matrixMultiply :: Num a => Matrix a -> Matrix a -> Matrix a
matrixMultiply = fmap . vectorMatrixMultiply