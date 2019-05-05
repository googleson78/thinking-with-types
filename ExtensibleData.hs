{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module ExtensibleData where

import Fcf (FindIndex, FromMaybe, Stuck, TyEq, type (=<<), Eval, type (@@))

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat, natVal)

import Unsafe.Coerce (unsafeCoerce)

-- Tried experimenting a bit:
-- 1) Member constraint on the constructor - it works
-- but it's kind of pointless because you (still) need to keep the index because of inj (I think),
-- so you are just introducing more overhead.
-- 2) Proxy instead of Int - why bother when it's still unsafe (citation needed).
--
-- Idea: A singleton could be used instead of an Int for "true" "proof".
data Sum (f :: k -> Type) (ts :: [t]) where
  UnsafeSum :: Show (f t) => Int -> f t -> Sum f ts

deriving instance Show (Sum f ts)

inj :: forall f t ts. (Member t ts, Show (f t))
    => f t -> Sum f ts
inj = UnsafeSum (index @t @ts)

index :: forall t ts. Member t ts => Int
index = fromIntegral $ natVal $ Proxy @(Eval (FindElem t ts))

proj :: forall f t ts. (Member t ts) => Sum f ts -> Maybe (f t)
proj (UnsafeSum i fx) =
  if i == index @t @ts
  then Just $ unsafeCoerce fx
  else Nothing

decompose :: Sum f (t ': ts)
          -> Either (f t)
                    (Sum f ts)
decompose (UnsafeSum 0 fx) = Left $ unsafeCoerce fx
decompose (UnsafeSum n fx) = Right $ UnsafeSum (pred n) fx

decompose' :: Sum f (t ': ts)
          -> Either (f t)
                    (Sum f ts)
decompose' s@(UnsafeSum i fx)
  = case proj s of
      Nothing -> Right $ UnsafeSum (pred i) fx
      Just x  -> Left x

weaken :: Sum f ts -> Sum f (t ': ts)
weaken (UnsafeSum n fx) = UnsafeSum (succ n) fx


type FindElem (key :: k) (ts :: [k]) =
  FromMaybe Stuck =<< FindIndex (TyEq key) ts

type Member t ts = KnownNat (Eval (FindElem t ts))
