{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
module WebApi.Versioning
       ( MajorMinor (..)
       , Major (..)
       , OrdVersion (..)
       , VersionOrd
       , SingOrd (..)
       , compareVersion
       ) where

import           Data.Proxy
import           GHC.TypeLits

class OrdVersion (ver :: *) where
  cmpVersion :: (ver ~ ((proxy :: k -> *) (v1 :: k)), ord ~ (VersionOrd (proxy v1) (proxy v2)), SingOrd ord) =>  proxy v1 -> proxy (v2 :: k) -> Proxy ord
  cmpVersion _ _ = (Proxy :: Proxy ord)

class SingOrd (ord :: Ordering) where
  singOrd :: proxy ord -> Ordering

instance SingOrd 'EQ where
  singOrd = const EQ

instance SingOrd 'LT where
  singOrd = const LT

instance SingOrd 'GT where
  singOrd = const GT

type family VersionOrd (v1 :: k) (v2 :: k) :: Ordering

compareVersion :: (OrdVersion (proxy v1), SingOrd (VersionOrd (proxy v1) (proxy v2))) => proxy (v1 :: k) -> proxy (v2 :: k) -> Ordering
compareVersion v1 v2 = singOrd $ cmpVersion v1 v2

data MajorMinor (ver :: (Nat, Nat)) = MajorMinor
data Major (maj :: Nat) = Major

type instance VersionOrd (Major (m :: Nat)) (Major (n :: Nat)) = (CmpNat m n)
type instance VersionOrd (MajorMinor '(maj1, min1)) (MajorMinor '(maj2, min2)) = 'EQ


instance OrdVersion (Major maj) where
instance OrdVersion (MajorMinor '(maj, min)) where
