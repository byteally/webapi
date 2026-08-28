{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
-- | The values an action's request (and result) is made of.
--
-- A 'Val' is a constant, a DL variable (an earlier action's result, looked
-- up when the action runs), a combination of the two, a record kept field
-- by field, or nothing at all ('Unset'). It is an 'Applicative' so
-- hand-written actions compose values with @<$>@ / @<*>@; the record node
-- ('Fields', over "GHC.Generics") is what lets a bridge (Dhall, …) replace
-- or fill single fields of a default request, and what makes an 'Unset'
-- leaf mean "this field, nobody supplied it" instead of poisoning the
-- whole request.
module Test.WebApi.Val
  ( Val (..)
  , GVal (..)
  , GValRep (..)
  , resolveVal
  , shrinkVal
  , fromVar
  , fields
  , setField
  , unsetField
  , unsetLabel
  , GSetField
  ) where

import Test.QuickCheck.StateModel (Var, LookUp, HasVariables (..), VarContext, Any, shrinkVar)
import qualified Data.Set as Set
import Data.Kind (Type)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import GHC.Generics
import GHC.TypeLits

data Val a where
  Const :: a -> Val a
  Var :: Typeable x => (x -> a) -> Var x -> Val a
  Pair :: ((x1, x2) -> a) -> (Val x1, Val x2) -> Val a
  -- | A function over a structured value (a 'Fields'); other nodes carry
  -- their own function.
  Map :: (x -> a) -> Val x -> Val a
  -- | A record, one 'Val' per field.
  Fields :: (Generic a, GValRep (Rep a)) => GVal (Rep a) -> Val a
  -- | Nobody supplied this leaf; the label names it (@Type.field@).
  Unset :: Text -> Val a

instance Functor Val where
  fmap f = \case
    Const a -> Const (f a)
    Var g v -> Var (f . g) v
    Pair g vs -> Pair (f . g) vs
    Map g v -> Map (f . g) v
    v@Fields {} -> Map f v
    Unset n -> Unset n

-- | Constants fold; anything symbolic becomes a 'Pair' so every leaf stays
-- visible to resolution, variable collection and shrinking. An 'Unset'
-- function has nothing to apply, so it stays 'Unset'.
instance Applicative Val where
  pure = Const
  Const f <*> a = fmap f a
  Unset n <*> _ = Unset n
  f <*> Const a = fmap ($ a) f
  f <*> a = Pair (\(g, x) -> g x) (f, a)

-- | 'Left' names the first 'Unset' leaf.
resolveVal :: LookUp -> Val a -> Either Text a
resolveVal lkp = \case
  Const a -> Right a
  Var f v -> Right (f (lkp v))
  Pair f (v1, v2) -> f <$> ((,) <$> resolveVal lkp v1 <*> resolveVal lkp v2)
  Map f v -> f <$> resolveVal lkp v
  Fields gv -> to <$> gResolve lkp gv
  Unset n -> Left n

instance HasVariables (Val a) where
  getAllVariables = \case
    Const {} -> mempty
    Var _ v -> getAllVariables v
    Pair _ (v1, v2) -> getAllVariables v1 <> getAllVariables v2
    Map _ v -> getAllVariables v
    Fields gv -> gVars gv
    Unset {} -> mempty

-- | Shrink one leaf at a time: a variable to an earlier one of its type
-- (see 'shrinkVar'); constants and unset leaves do not shrink.
shrinkVal :: VarContext -> Val a -> [Val a]
shrinkVal vctx = \case
  Const {} -> []
  Var f v -> Var f <$> shrinkVar vctx v
  Pair f (v1, v2) -> [Pair f (v1', v2) | v1' <- shrinkVal vctx v1] ++ [Pair f (v1, v2') | v2' <- shrinkVal vctx v2]
  Map f v -> Map f <$> shrinkVal vctx v
  Fields gv -> Fields <$> gShrink vctx gv
  Unset {} -> []

fromVar :: Typeable a => Var a -> Val a
fromVar = Var id

-- | A record kept field by field, every field a constant to begin with.
fields :: (Generic a, GValRep (Rep a)) => a -> Val a
fields = Fields . gConst . from

-- | Replace one field (by its selector name) of a record value.
setField :: forall name x a. (Generic a, GValRep (Rep a), GSetField name x (Rep a)) => Val x -> Val a -> Val a
setField v = \case
  Fields gv -> Fields (gSetField @name v gv)
  Const a -> Fields (gSetField @name v (gConst (from a)))
  other -> Pair (\(r, x) -> to (gSetRep @name x (from r))) (other, v)

-- | Leave one field (by its selector name) unset: the request cannot be
-- performed until something (a script override, a model default) fills it.
unsetField :: forall name x a. (Typeable a, KnownSymbol name, Generic a, GValRep (Rep a), GSetField name x (Rep a)) => Val a -> Val a
unsetField = setField @name (Unset (unsetLabel @name @a))

unsetLabel :: forall name a. (Typeable a, KnownSymbol name) => Text
unsetLabel = T.pack (show (typeRep (Proxy @a))) <> "." <> T.pack (symbolVal (Proxy @name))

-- | The 'Rep' of a record with a 'Val' at every field.
data GVal (f :: Type -> Type) where
  GM1 :: GVal f -> GVal (M1 i c f)
  GProd :: GVal f -> GVal g -> GVal (f :*: g)
  GK1 :: Val a -> GVal (K1 i a)
  GU1 :: GVal U1

class GValRep f where
  gConst :: f p -> GVal f
  gResolve :: LookUp -> GVal f -> Either Text (f p)
  gVars :: GVal f -> Set.Set (Any Var)
  gShrink :: VarContext -> GVal f -> [GVal f]

instance GValRep f => GValRep (M1 i c f) where
  gConst (M1 f) = GM1 (gConst f)
  gResolve lkp (GM1 g) = M1 <$> gResolve lkp g
  gVars (GM1 g) = gVars g
  gShrink vc (GM1 g) = GM1 <$> gShrink vc g

instance (GValRep f, GValRep g) => GValRep (f :*: g) where
  gConst (f :*: g) = GProd (gConst f) (gConst g)
  gResolve lkp (GProd a b) = (:*:) <$> gResolve lkp a <*> gResolve lkp b
  gVars (GProd a b) = gVars a <> gVars b
  gShrink vc (GProd a b) = [GProd a' b | a' <- gShrink vc a] ++ [GProd a b' | b' <- gShrink vc b]

instance GValRep (K1 i a) where
  gConst (K1 a) = GK1 (Const a)
  gResolve lkp (GK1 v) = K1 <$> resolveVal lkp v
  gVars (GK1 v) = getAllVariables v
  gShrink vc (GK1 v) = GK1 <$> shrinkVal vc v

instance GValRep U1 where
  gConst U1 = GU1
  gResolve _ GU1 = Right U1
  gVars GU1 = mempty
  gShrink _ GU1 = []

-- | Set a field by selector name, in a 'GVal' or a plain 'Rep' value.
class GSetField (name :: Symbol) (x :: Type) (f :: Type -> Type) | name f -> x where
  gSetField :: Val x -> GVal f -> GVal f
  gSetRep :: x -> f p -> f p

instance GSetField name x f => GSetField name x (D1 c f) where
  gSetField v (GM1 g) = GM1 (gSetField @name v g)
  gSetRep x (M1 f) = M1 (gSetRep @name x f)

instance GSetField name x f => GSetField name x (C1 c f) where
  gSetField v (GM1 g) = GM1 (gSetField @name v g)
  gSetRep x (M1 f) = M1 (gSetRep @name x f)

instance (FieldIn name f ~ inLeft, GSetFieldProd inLeft name x f g) => GSetField name x (f :*: g) where
  gSetField = gSetFieldProd @inLeft @name
  gSetRep = gSetRepProd @inLeft @name

instance (x ~ x') => GSetField name x (S1 ('MetaSel ('Just name) su ss ds) (K1 i x')) where
  gSetField v (GM1 (GK1 _)) = GM1 (GK1 v)
  gSetRep x (M1 (K1 _)) = M1 (K1 x)

class GSetFieldProd (inLeft :: Bool) (name :: Symbol) (x :: Type) (f :: Type -> Type) (g :: Type -> Type) | name f g -> x where
  gSetFieldProd :: Val x -> GVal (f :*: g) -> GVal (f :*: g)
  gSetRepProd :: x -> (f :*: g) p -> (f :*: g) p

instance GSetField name x f => GSetFieldProd 'True name x f g where
  gSetFieldProd v (GProd a b) = GProd (gSetField @name v a) b
  gSetRepProd x (a :*: b) = gSetRep @name x a :*: b

instance GSetField name x g => GSetFieldProd 'False name x f g where
  gSetFieldProd v (GProd a b) = GProd a (gSetField @name v b)
  gSetRepProd x (a :*: b) = a :*: gSetRep @name x b

type family FieldIn (name :: Symbol) (f :: Type -> Type) :: Bool where
  FieldIn name (S1 ('MetaSel ('Just name) su ss ds) k) = 'True
  FieldIn name (S1 _ _) = 'False
  FieldIn name (M1 i c f) = FieldIn name f
  FieldIn name (f :*: g) = Or (FieldIn name f) (FieldIn name g)
  FieldIn name _ = 'False

type family Or (a :: Bool) (b :: Bool) :: Bool where
  Or 'True _ = 'True
  Or 'False b = b
