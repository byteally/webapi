{-|
Module      : WebApi.Versioning
License     : BSD3
Stability   : experimental
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
module WebApi.Versioning
       (
       -- * Versioning styles
         MajorMinor (..)
       , Major (..)

       -- * Building custom versions  
       , OrdVersion (..)
       , VersionOrd
       , compareVersion
       ) where

import           Data.Proxy
import           GHC.TypeLits

-- | Comparison between versions.
class OrdVersion (ver :: *) where
  cmpVersion :: (ver ~ ((proxy :: k -> *) (v1 :: k)), ord ~ (VersionOrd (proxy v1) (proxy v2)), SingOrd ord) =>  proxy v1 -> proxy (v2 :: k) -> Proxy ord
  cmpVersion _ _ = (Proxy :: Proxy ord)

-- | Singleton class for ordering
class SingOrd (ord :: Ordering) where
  singOrd :: proxy ord -> Ordering

instance SingOrd 'EQ where
  singOrd = const EQ

instance SingOrd 'LT where
  singOrd = const LT

instance SingOrd 'GT where
  singOrd = const GT

-- | Defines ordering of versions.
type family VersionOrd (v1 :: k) (v2 :: k) :: Ordering

-- | Comparison between two versions. Returns an 'Ord'.
-- > compareVersion (MajorMinor :: MajorMinor (0, 0)) (MajorMinor :: MajorMinor (0, 1)) == LT             
compareVersion :: (OrdVersion (proxy v1), SingOrd (VersionOrd (proxy v1) (proxy v2))) => proxy (v1 :: k) -> proxy (v2 :: k) -> Ordering
compareVersion v1 v2 = singOrd $ cmpVersion v1 v2

-- | A Style of versioning which has a Major version and a Minor version.
data MajorMinor (ver :: (Nat, Nat)) = MajorMinor

-- | A Style of versioning which has only has a Major version.
data Major (maj :: Nat) = Major

type instance VersionOrd (Major (m :: Nat)) (Major (n :: Nat)) = (CmpNat m n)
type instance VersionOrd (MajorMinor '(maj1, min1)) (MajorMinor '(maj2, min2)) = 'EQ


instance OrdVersion (Major maj) where
instance OrdVersion (MajorMinor '(maj, min)) where
