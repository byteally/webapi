{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module WebApiInstances where

import Control.Applicative
import Data.Aeson
import Data.Int
import Data.Text (Text)
import Data.Vector as V
import GHC.Generics (Generic)
import OpenApiModels
import WebApi.Contract
import WebApi.Param

data Untyped = Maybe Text
type CurrencyGETR = NetSuite :// ("currency" :/ Int32)
type CurrencyGETRPath = "currency" :/ Int32
type CurrencyPUTR = NetSuite :// ("currency" :/ Text)
type CurrencyPUTRPath = "currency" :/ Text
type CurrencyDELETER = NetSuite :// ("currency" :/ Int32)
type CurrencyDELETERPath = "currency" :/ Int32
type CurrencyPATCHR = NetSuite :// ("currency" :/ Int32)
type CurrencyPATCHRPath = "currency" :/ Int32
data GetCurrencyQP
    = GetCurrencyQP
    { expandSubResources :: (Maybe Bool)
    , fields :: (Maybe Text)
    , simpleEnumFormat :: (Maybe Bool)
    }
    deriving (Show, Eq, Generic)
data PutCurrencyQP
    = PutCurrencyQP
    { replaceSelectedFields :: (Maybe Bool)
    , replace :: (Maybe Text)
    }
    deriving (Show, Eq, Generic)
data PatchCurrencyQP
    = PatchCurrencyQP
    { replaceSelectedFields :: (Maybe Bool)
    , replace :: (Maybe Text)
    }
    deriving (Show, Eq, Generic)
type CurrencyR = NetSuite :// "currency"
type CurrencyRPath = "currency"
data ListCurrenciesQP
    = ListCurrenciesQP
    { offset :: (Maybe Int32)
    , limit :: (Maybe Int32)
    , q :: (Maybe Text)
    }
    deriving (Show, Eq, Generic)
data PostCurrencyQP
    = PostCurrencyQP {replace :: (Maybe Text)}
    deriving (Show, Eq, Generic)
data NetSuite
instance WebApi NetSuite where
    type
        Apis NetSuite =
            '[ Route '[GET] CurrencyGETR
             , Route '[PUT] CurrencyPUTR
             , Route '[DELETE] CurrencyDELETER
             , Route '[PATCH] CurrencyPATCHR
             , Route '[GET, POST] CurrencyR
             ]
instance ApiContract NetSuite GET CurrencyGETR where
    type OperationId GET CurrencyGETR = 'OpId NetSuite "getCurrency"
    type QueryParam GET CurrencyGETR = GetCurrencyQP
    type ApiOut GET CurrencyGETR = Currency
    type ApiErr GET CurrencyGETR = NsError
instance ApiContract NetSuite PUT CurrencyPUTR where
    type OperationId PUT CurrencyPUTR = 'OpId NetSuite "putCurrency"
    type QueryParam PUT CurrencyPUTR = PutCurrencyQP
    type RequestBody PUT CurrencyPUTR = '[Currency]
    type ApiOut PUT CurrencyPUTR = NsError
    type ApiErr PUT CurrencyPUTR = NsError
instance ApiContract NetSuite DELETE CurrencyDELETER where
    type OperationId DELETE CurrencyDELETER = 'OpId NetSuite "deleteCurrency"
    type ApiOut DELETE CurrencyDELETER = NsError
    type ApiErr DELETE CurrencyDELETER = NsError
instance ApiContract NetSuite PATCH CurrencyPATCHR where
    type OperationId PATCH CurrencyPATCHR = 'OpId NetSuite "patchCurrency"
    type QueryParam PATCH CurrencyPATCHR = PatchCurrencyQP
    type RequestBody PATCH CurrencyPATCHR = '[Currency]
    type ApiOut PATCH CurrencyPATCHR = NsError
    type ApiErr PATCH CurrencyPATCHR = NsError
instance ApiContract NetSuite GET CurrencyR where
    type OperationId GET CurrencyR = 'OpId NetSuite "listCurrencies"
    type QueryParam GET CurrencyR = ListCurrenciesQP
    type ApiOut GET CurrencyR = CurrencyCollection
    type ApiErr GET CurrencyR = NsError
instance ApiContract NetSuite POST CurrencyR where
    type OperationId POST CurrencyR = 'OpId NetSuite "postCurrency"
    type QueryParam POST CurrencyR = PostCurrencyQP
    type RequestBody POST CurrencyR = '[Currency]
    type ApiOut POST CurrencyR = NsError
    type ApiErr POST CurrencyR = NsError
instance ToParam 'QueryParam GetCurrencyQP
instance FromParam 'QueryParam GetCurrencyQP
instance ToParam 'QueryParam PutCurrencyQP
instance FromParam 'QueryParam PutCurrencyQP
instance ToParam 'QueryParam PatchCurrencyQP
instance FromParam 'QueryParam PatchCurrencyQP
instance ToParam 'QueryParam ListCurrenciesQP
instance FromParam 'QueryParam ListCurrenciesQP
instance ToParam 'QueryParam PostCurrencyQP
instance FromParam 'QueryParam PostCurrencyQP
