{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleContexts      #-}

module WebApi.Util where

import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import GHC.TypeLits
import GHC.Exts
import Data.Kind

type family HListToTuple (xs :: [Type]) :: Type where
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

type family HListToRecTuple (xs :: [Type]) :: Type where
  HListToRecTuple (x ': xs)                = (x, HListToRecTuple xs)
  HListToRecTuple '[]                      = ()

class ToHListRecTuple (xs :: [Type]) where
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

type family StripContents (a :: [Type]) :: [Type] where
  StripContents (t ': ts) = StripContent t ': StripContents ts
  StripContents '[]       = '[]

type family StripContent a where
  StripContent (Content ctypes t) = t 
  StripContent t                  = t

data Content (ctypes :: [Type]) (a :: Type)


-- | Datatype representing a endpoint.
data Route (ms :: [Type]) (r :: Type)

-- | Datatype associating a namespace with a route.
data (://) (ns :: Type) (ps :: k)
infixr 5 ://

-- | Datatype representing a route.
data (:/) (p1 :: k) (p2 :: k1)
infixr 5 :/

-- | Datatype representing a static path piece.
data Static (s :: Symbol)

type Root = Static ""

  
instance (MkFormatStr (ToPieces (a :/ b))) => MkPathFormatString (a :/ b) where
  mkPathFormatString _ = mkFormatStr (Proxy :: Proxy (ToPieces (a :/ b)))

instance (MkPathFormatString b) => MkPathFormatString (a :// (b :: Type)) where
  mkPathFormatString _ = mkPathFormatString (Proxy :: Proxy b)

instance (MkPathFormatString (Static b)) => MkPathFormatString (a :// (b :: Symbol)) where
  mkPathFormatString _ = mkPathFormatString (Proxy :: Proxy (Static b))

instance (KnownSymbol s) => MkPathFormatString (Static s) where
  mkPathFormatString _ = mkFormatStr (Proxy :: Proxy (ToPieces (Static s)))


data Namespace (ns :: Type)

-- | Convert the path into a flat hierarchy.
type family ToPieces (r :: k) :: [Type] where
  ToPieces (ns :// (ps :: Type))      = Namespace ns ': ToPieces' ps
  ToPieces (ns :// (ps :: Symbol)) = Namespace ns ': ToPieces' (Static ps)
  ToPieces p                       = ToPieces' p

type family ToPieces' (r :: k) :: [Type] where
  ToPieces' (Static s)                         = '[StaticPiece s]
  ToPieces' ((p1 :: Symbol) :/ (p2 :: Symbol)) = '[StaticPiece p1, StaticPiece p2]
  ToPieces' ((p1 :: Type) :/ (p2 :: Symbol))      = '[DynamicPiece p1, StaticPiece p2]
  ToPieces' ((p1 :: Symbol) :/ (p2 :/ p3))     = StaticPiece p1 ': ToPieces' (p2 :/ p3)
  ToPieces' ((p1 :: Type) :/ (p2 :/ p3))          = DynamicPiece p1 ': ToPieces' (p2 :/ p3)
  ToPieces' ((p1 :: Type) :/ (p2 :: Type))           = '[DynamicPiece p1, DynamicPiece p2]
  ToPieces' ((p1 :: Symbol) :/ (p2 :: Type))      = '[StaticPiece p1, DynamicPiece p2]

type family FromPieces (pps :: [Type]) :: Type where
  FromPieces (Namespace ns ': ps) = ns :// FromPieces' ps
  FromPieces ps                   = FromPieces' ps

type family FromPieces' (pps :: [Type]) :: Type where
  FromPieces' '[StaticPiece s]                    = Static s
  FromPieces' '[StaticPiece p1, StaticPiece p2]   = p1 :/ p2
  FromPieces' '[DynamicPiece p1, DynamicPiece p2] = p1 :/ p2
  FromPieces' '[StaticPiece p1, DynamicPiece p2]  = p1 :/ p2
  FromPieces' '[DynamicPiece p1, StaticPiece p2]  = p1 :/ p2

  FromPieces' (DynamicPiece p ': ps)              = p :/ FromPieces' ps
  FromPieces' (StaticPiece p ': ps)               = p :/ FromPieces' ps
-- | Type of segments of a Path.
data PathSegment = StaticSegment Text -- ^ A static segment
                 | Hole -- ^ A dynamic segment
                 deriving (Show, Eq)

-- | Describe representation of the route.
class MkPathFormatString r where
  -- | Given a route, this function should produce the @[PathSegment]@ of that route. This gives the flexibility to hook in a different routing system into the application.
  mkPathFormatString :: Proxy r -> [PathSegment]

class MkFormatStr (xs :: [Type]) where
  mkFormatStr :: Proxy xs -> [PathSegment]

instance MkFormatStr '[] where
  mkFormatStr _ = []  


data StaticPiece (s :: Symbol)

instance (KnownSymbol s, MkFormatStr xs) => MkFormatStr (StaticPiece s ': xs) where
  mkFormatStr _ = StaticSegment (pack (symbolVal (Proxy :: Proxy s))) : mkFormatStr (Proxy :: Proxy xs)

data DynamicPiece (t :: Type)

instance (MkFormatStr xs) => MkFormatStr (DynamicPiece s ': xs) where
  mkFormatStr _ = Hole : mkFormatStr (Proxy :: Proxy xs)

type family FilterDynP (ps :: [Type]) :: [Type] where
  FilterDynP (DynamicPiece p1 ': p2) = p1 ': FilterDynP p2
  FilterDynP (p1 ': p2)              = FilterDynP p2
  FilterDynP '[]                     = '[]

type family Elem t ts :: Constraint where
  Elem t ts = Elem' t ts ts

type family Elem' t ts ots :: Constraint where
  Elem' t (t ': ts) _   = ()
  Elem' t (_ ': ts) ots = Elem' t ts ots
  Elem' t '[]       ots = TypeError ('Text "Type " ':<>:
                                     'ShowType t   ':<>:
                                     'Text " is not a member of " ':<>:
                                     'ShowType ots
                                    )
