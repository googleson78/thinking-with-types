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
  UnsafeSum :: Int -> t -> Sum f ts

inj :: forall f t ts. Member t ts
    => t -> Sum f ts
inj = UnsafeSum (index @t @ts)

index :: forall t ts. Member t ts => Int
index = fromIntegral $ natVal $ Proxy @(Eval (FindElem t ts))

proj :: forall f t ts. (Member t ts) => Sum f ts -> Maybe (f t)
proj (UnsafeSum i x) =
  if i == index @t @ts
  then unsafeCoerce $ Just x
  else Nothing

type FindElem (key :: k) (ts :: [k]) =
  FromMaybe Stuck =<< FindIndex (TyEq key) ts

type Member t ts = KnownNat (Eval (FindElem t ts))
