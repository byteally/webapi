-- |

{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE CPP                       #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses   #-}
{-# LANGUAGE FlexibleInstances         #-}
#endif



module WebApi.Docs
       ( WebApiDocs (..)
       , ApiDocs (..)
       , Docs
       , FName
       , Rec(..), nil
       , ResourceDoc (..)
       , DocField
       , (:-)
       , bodyDocs
       , docs
       , field
       , nested
       , (-:)
       , docSummary
       , nestedSummary
       , nestedDocs
       , f
       ) where

import WebApi.Contract
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import GHC.Exts
#if __GLASGOW_HASKELL__ >= 800
import GHC.OverloadedLabels
#endif
import Data.Text
import Data.String
import Language.Haskell.TH.Quote
import Language.Haskell.TH

class (WebApi p)  => WebApiDocs (p :: *) where
  type DocumentedApis p :: [*]

data ResourceDoc m r = ResourceDoc
                     deriving (Generic)

class ApiContract p m r => ApiDocs (p :: *) (m :: *) (r :: *) where
  apiDocs :: Proxy p -> Proxy (Request m r) -> Docs (ResourceDoc m r)

  default apiDocs :: Generic (ResourceDoc m r) => Proxy p -> Proxy (Request m r) -> Docs (ResourceDoc m r)
  apiDocs _ _ = docs "" nil

  queryParamDocs :: Generic (QueryParam m r) => Proxy p -> Proxy (Request m r) -> Docs (QueryParam m r)

  default queryParamDocs :: Generic (QueryParam m r) => Proxy p -> Proxy (Request m r) -> Docs (QueryParam m r)
  queryParamDocs _ _ = docs "" nil

  formParamDocs :: Generic (FormParam m r) => Proxy p -> Proxy (Request m r) -> Docs (FormParam m r)

  default formParamDocs :: Generic (FormParam m r) => Proxy p -> Proxy (Request m r) -> Docs (FormParam m r)
  formParamDocs _ _ = docs "" nil

  fileParamDocs :: Generic (FileParam m r) => Proxy p -> Proxy (Request m r) -> Docs (FileParam m r)

  default fileParamDocs :: Generic (FileParam m r) => Proxy p -> Proxy (Request m r) -> Docs (FileParam m r)
  fileParamDocs _ _ = docs "" nil

  headerInDocs :: Generic (HeaderIn m r) => Proxy p -> Proxy (Request m r) -> Docs (HeaderIn m r)

  default headerInDocs :: Generic (HeaderIn m r) => Proxy p -> Proxy (Request m r) -> Docs (HeaderIn m r)
  headerInDocs _ _ = docs "" nil

  cookieInDocs :: Generic (CookieIn m r) => Proxy p -> Proxy (Request m r) -> Docs (CookieIn m r)

  default cookieInDocs :: Generic (CookieIn m r) => Proxy p -> Proxy (Request m r) -> Docs (CookieIn m r)
  cookieInDocs _ _ = docs "" nil

  apiOutDocs :: Generic (ApiOut m r) => Proxy p -> Proxy (Request m r) -> Docs (ApiOut m r)

  default apiOutDocs :: Generic (ApiOut m r) => Proxy p -> Proxy (Request m r) -> Docs (ApiOut m r)
  apiOutDocs _ _ = docs "" nil

  apiErrDocs :: Generic (ApiErr m r) => Proxy p -> Proxy (Request m r) -> Docs (ApiErr m r)

  default apiErrDocs :: Generic (ApiErr m r) => Proxy p -> Proxy (Request m r) -> Docs (ApiErr m r)
  apiErrDocs _ _ = docs "" nil

  headerOutDocs :: Generic (HeaderOut m r) => Proxy p -> Proxy (Request m r) -> Docs (HeaderOut m r)

  default headerOutDocs :: Generic (HeaderOut m r) => Proxy p -> Proxy (Request m r) -> Docs (HeaderOut m r)
  headerOutDocs _ _ = docs "" nil

  cookieOutDocs :: Generic (CookieOut m r) => Proxy p -> Proxy (Request m r) -> Docs (CookieOut m r)

  default cookieOutDocs :: Generic (CookieOut m r) => Proxy p -> Proxy (Request m r) -> Docs (CookieOut m r)
  cookieOutDocs _ _ = docs "" nil

  reqBodyDocs :: All1 Generic (RequestBody m r) => Proxy p -> Proxy (Request m r) -> ReqBodyDoc (RequestBody m r)
  default reqBodyDocs :: ( EmptyReqBodyDoc (RequestBody m r)
                        , All1 Generic (RequestBody m r)
                        ) => Proxy p -> Proxy (Request m r) -> ReqBodyDoc (RequestBody m r)
  reqBodyDocs _ _ = emptyBodyDoc Proxy


type family All1 (c :: * -> Constraint) (xs :: [*]) :: Constraint where
  All1 c (x ': xs) = (c x, All1 c xs)
  All1 c '[]       = ()

data ReqBodyDoc (bodies :: [*]) = ReqBodyDoc (Rec Docs bodies)

bodyDocs :: Rec (DocField body) xs -> ReqBodyDoc '[body]
bodyDocs bdocs = ReqBodyDoc (docs "" bdocs :& nil)

class EmptyReqBodyDoc (bodies :: [*]) where
  emptyBodyDoc :: Proxy bodies -> ReqBodyDoc bodies

instance EmptyReqBodyDoc bodies => EmptyReqBodyDoc (bdy ': bodies) where
  emptyBodyDoc _ = case emptyBodyDoc Proxy of
    ReqBodyDoc docss -> ReqBodyDoc (docs "" nil :& docss)

instance EmptyReqBodyDoc '[] where
  emptyBodyDoc _ = ReqBodyDoc nil


data ((fn :: Symbol) :- (a :: *)) = Field

data Docs t = forall xs.Docs Text (Rec (DocField t) xs)

docSummary :: Docs t -> Text
docSummary (Docs summ _) = summ

data DocField s (fld :: *) where
  DocField :: FName fn -> Doc t -> DocField s (fn :- t)

data Doc t = Doc Text
           | Nested (NestedDoc t)

data NestedDoc t = NestedDoc
  { nsummary :: Text
  , ndocs    :: Docs t
  }

nestedDocs :: Doc t -> Maybe (Docs t)
nestedDocs (Nested ndoc) = Just $ ndocs ndoc
nestedDocs (Doc _)       = Nothing

nestedSummary :: Doc t -> Maybe Text
nestedSummary (Nested ndoc) = Just $ nsummary ndoc
nestedSummary _             = Nothing

instance IsString (Doc t) where
  fromString = Doc . pack

data Rec :: (k -> *) -> [k] -> * where
  Nil  :: Rec f '[]
  (:&) :: f x -> Rec f xs -> Rec f (x ': xs)

infixr 7 :&

data FName (fn :: Symbol) = FN

field :: forall fn.FName fn
field = FN

(-:) :: FName fn -> Doc t -> DocField s (fn :- t)
(-:) fn doc = DocField fn doc

docs :: Text -> Rec (DocField t) xs -> Docs t
docs = Docs

nested :: Text -> Docs t -> Doc t
nested summary = Nested . NestedDoc summary

nil :: Rec f '[]
nil = Nil

f :: QuasiQuoter
f = QuasiQuoter
  { quoteExp  = quoteFieldExp
  , quotePat  = error "Field QuasiQuote cannot be used in pattern"
  , quoteDec  = error "Field QuasiQuote cannot be used in declaration"
  , quoteType = error "Field QuasiQuote cannot be used in type"
  }

quoteFieldExp :: String -> Q Exp
quoteFieldExp fld = do
  let ty = litT $ strTyLit fld
  [|field :: FName $(ty) |]


#if __GLASGOW_HASKELL__ >= 800
instance fn ~ fn' => IsLabel (fn :: Symbol) (FName fn') where
#if __GLASGOW_HASKELL__ >= 800
  fromLabel = FN
#else
  fromLabel _ = FN
#endif
  {-# INLINE fromLabel #-}
#endif
