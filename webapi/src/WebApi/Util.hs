{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PolyKinds             #-}

module WebApi.Util where

import Data.Proxy (Proxy)

type family HListToTuple (xs :: [*]) :: * where
  HListToTuple '[]   = ()
  HListToTuple '[p1] = p1
  HListToTuple '[p1, p2] = (p1, p2)
  HListToTuple '[p1, p2, p3] = (p1, p2, p3)
  HListToTuple '[p1, p2, p3, p4] = (p1, p2, p3, p4)
  HListToTuple '[p1, p2, p3, p4, p5] = (p1, p2, p3, p4, p5)
  HListToTuple '[p1, p2, p3, p4, p5, p6] = (p1, p2, p3, p4, p5, p6)
  HListToTuple '[p1, p2, p3, p4, p5, p6, p7] = (p1, p2, p3, p4, p5, p6, p7)
  HListToTuple '[p1, p2, p3, p4, p5, p6, p7, p8] = (p1, p2, p3, p4, p5, p6, p7, p8)
  HListToTuple '[p1, p2, p3, p4, p5, p6, p7, p8, p9] = (p1, p2, p3, p4, p5, p6, p7, p8, p9)

type family HListToRecTuple (xs :: [*]) :: * where
  HListToRecTuple (x ': xs)                = (x, HListToRecTuple xs)
  HListToRecTuple '[]                      = ()

class ToHListRecTuple (xs :: [*]) where
  toRecTuple :: Proxy xs -> HListToTuple xs -> HListToRecTuple xs
  fromRecTuple :: Proxy xs -> HListToRecTuple xs -> HListToTuple xs

instance ToHListRecTuple '[] where
  toRecTuple _ () = ()
  fromRecTuple _ () = ()

instance (HListToRecTuple '[p1] ~ (p1, ())) => ToHListRecTuple '[p1] where
  toRecTuple _ (p1) = (p1, ())
  fromRecTuple _ (p1, ()) = (p1)

instance ToHListRecTuple '[p1, p2] where
  toRecTuple _ (p1, p2) = (p1, (p2, ()))
  fromRecTuple _ (p1, (p2, ())) = (p1, p2)

instance ToHListRecTuple '[p1, p2, p3] where
  toRecTuple _ (p1, p2, p3) = (p1, (p2, (p3, ())))
  fromRecTuple _ (p1, (p2, (p3, ()))) = (p1, p2, p3)

instance ToHListRecTuple '[p1, p2, p3, p4] where
  toRecTuple _ (p1, p2, p3, p4) = (p1, (p2, (p3, (p4, ()))))
  fromRecTuple _ (p1, (p2, (p3, (p4, ())))) = (p1, p2, p3, p4)

instance ToHListRecTuple '[p1, p2, p3, p4, p5] where
  toRecTuple _ (p1, p2, p3, p4, p5) = (p1, (p2, (p3, (p4, (p5, ())))))
  fromRecTuple _ (p1, (p2, (p3, (p4, (p5, ()))))) = (p1, p2, p3, p4, p5)

instance ToHListRecTuple '[p1, p2, p3, p4, p5, p6] where
  toRecTuple _ (p1, p2, p3, p4, p5, p6) = (p1, (p2, (p3, (p4, (p5, (p6, ()))))))
  fromRecTuple _ (p1, (p2, (p3, (p4, (p5, (p6, ())))))) = (p1, p2, p3, p4, p5, p6)

instance ToHListRecTuple '[p1, p2, p3, p4, p5, p6, p7] where
  toRecTuple _ (p1, p2, p3, p4, p5, p6, p7) = (p1, (p2, (p3, (p4, (p5, (p6, (p7, ())))))))
  fromRecTuple _ (p1, (p2, (p3, (p4, (p5, (p6, (p7, ()))))))) = (p1, p2, p3, p4, p5, p6, p7)

instance ToHListRecTuple '[p1, p2, p3, p4, p5, p6, p7, p8] where
  toRecTuple _ (p1, p2, p3, p4, p5, p6, p7, p8) = (p1, (p2, (p3, (p4, (p5, (p6, (p7, (p8, ()))))))))
  fromRecTuple _ (p1, (p2, (p3, (p4, (p5, (p6, (p7, (p8, ())))))))) = (p1, p2, p3, p4, p5, p6, p7, p8)

instance ToHListRecTuple '[p1, p2, p3, p4, p5, p6, p7, p8, p9] where
  toRecTuple _ (p1, p2, p3, p4, p5, p6, p7, p8, p9) = (p1, (p2, (p3, (p4, (p5, (p6, (p7, (p8, (p9, ())))))))))
  fromRecTuple _ (p1, (p2, (p3, (p4, (p5, (p6, (p7, (p8, (p9, ()))))))))) = (p1, p2, p3, p4, p5, p6, p7, p8, p9)

infixr 5 :++
type family (:++) (as :: [k]) (bs :: [k]) :: [k] where
  '[] :++ bs       = bs
  (a ': as) :++ bs = a ': (as :++ bs)
