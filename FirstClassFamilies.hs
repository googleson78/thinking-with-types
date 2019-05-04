{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module FirstClassFamilies where

import Data.Kind

import GHC.TypeLits

type Exp a = a -> Type

data ListToMaybe :: [Maybe t] -> Exp [t]

type family Eval (e :: Exp a) :: a

type instance Eval
  (ListToMaybe '[]) = '[]
type instance Eval
  (ListToMaybe (Nothing ': xs)) = Eval (ListToMaybe xs)
type instance Eval
  (ListToMaybe (Just x ': xs)) = x ': Eval (ListToMaybe xs)

data MapList :: (a -> Exp b) -> [a] -> Exp [b]

type instance Eval
  (MapList _1 '[]) = '[]
type instance Eval
  (MapList f (x ': xs)) = Eval (f x) ': Eval (MapList f xs)

data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b

type instance Eval
  (Foldr _1 v '[]) = v
type instance Eval
  (Foldr f v (x ': xs)) = Eval (f x (Eval (Foldr f v xs)))

data (:+:) :: Nat -> Nat -> Exp Nat

type instance Eval
  (n :+: m) = n + m
