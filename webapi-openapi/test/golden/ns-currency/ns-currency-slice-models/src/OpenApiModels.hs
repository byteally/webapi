{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module OpenApiModels where

import Control.Applicative
import Data.Aeson
import Data.Int
import Data.Text (Text)
import Data.Vector as V
import GHC.Generics (Generic)

data CurrencyCollection
    = CurrencyCollection
    { offset :: (Maybe Int64)
    , totalResults :: (Maybe Int64)
    , links :: (Vector NsLink)
    , count :: (Maybe Int64)
    , hasMore :: (Maybe Bool)
    , items :: (Vector Currency)
    }
    deriving (Show, Eq, Generic)
instance FromJSON CurrencyCollection where
    parseJSON =
        withObject "CurrencyCollection"
            $ ( \v ->
                    ( ( ( ( (CurrencyCollection <$> (v .:? "offset"))
                                <*> (v .:? "totalResults")
                          )
                            <*> ((v .:? "links") .!= V.empty)
                        )
                            <*> (v .:? "count")
                      )
                        <*> (v .:? "hasMore")
                    )
                        <*> ((v .:? "items") .!= V.empty)
              )
instance ToJSON CurrencyCollection where
    toJSON
        (CurrencyCollection offset totalResults links count hasMore items) =
            object
                [ "offset" .= offset
                , "totalResults" .= totalResults
                , "links" .= links
                , "count" .= count
                , "hasMore" .= hasMore
                , "items" .= items
                ]
data NsLink
    = NsLink {href :: (Maybe Text), rel :: (Maybe Text)}
    deriving (Show, Eq, Generic)
instance FromJSON NsLink where
    parseJSON =
        withObject "NsLink"
            $ (\v -> (NsLink <$> (v .:? "href")) <*> (v .:? "rel"))
instance ToJSON NsLink where
    toJSON (NsLink href rel) = object ["href" .= href, "rel" .= rel]
data NsObjRefNameId
    = NsObjRefNameId {refName :: (Maybe Text), id :: (Maybe Text)}
    deriving (Show, Eq, Generic)
data Currency
    = Currency
    { lastModifiedDate :: (Maybe Text)
    , refName :: (Maybe Text)
    , exchangeRate :: (Maybe Double)
    , id :: (Maybe Text)
    , isAnchorCurrency :: (Maybe Bool)
    , links :: (Vector NsLink)
    , symbol :: (Maybe Text)
    , locale :: (Maybe NsObjRefNameId)
    , includeInFxRateUpdates :: (Maybe Bool)
    , displaySymbol :: (Maybe Text)
    , symbolPlacement :: (Maybe NsObjRefNameId)
    , name :: (Maybe Text)
    , overrideCurrencyFormat :: (Maybe Bool)
    , currencyPrecision :: (Maybe Int64)
    , fxRateUpdateTimezone :: (Maybe NsObjRefNameId)
    , isInactive :: (Maybe Bool)
    , formatSample :: (Maybe Text)
    , externalId :: (Maybe Text)
    , isBaseCurrency :: (Maybe Bool)
    }
    deriving (Show, Eq, Generic)
instance FromJSON NsObjRefNameId where
    parseJSON =
        withObject "NsObjRefNameId"
            $ (\v -> (NsObjRefNameId <$> (v .:? "refName")) <*> (v .:? "id"))
instance ToJSON NsObjRefNameId where
    toJSON (NsObjRefNameId refName id) =
        object ["refName" .= refName, "id" .= id]
instance FromJSON Currency where
    parseJSON =
        withObject "Currency"
            $ ( \v ->
                    ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Currency <$> (v .:? "lastModifiedDate"))
                                                        <*> (v .:? "refName")
                                                    )
                                                        <*> (v .:? "exchangeRate")
                                                  )
                                                    <*> (v .:? "id")
                                                )
                                                    <*> (v .:? "isAnchorCurrency")
                                              )
                                                <*> ((v .:? "links") .!= V.empty)
                                            )
                                                <*> (v .:? "symbol")
                                          )
                                            <*> (v .:? "locale")
                                        )
                                            <*> (v .:? "includeInFxRateUpdates")
                                      )
                                        <*> (v .:? "displaySymbol")
                                    )
                                        <*> (v .:? "symbolPlacement")
                                  )
                                    <*> (v .:? "name")
                                )
                                    <*> (v .:? "overrideCurrencyFormat")
                              )
                                <*> (v .:? "currencyPrecision")
                            )
                                <*> (v .:? "fxRateUpdateTimezone")
                          )
                            <*> (v .:? "isInactive")
                        )
                            <*> (v .:? "formatSample")
                      )
                        <*> (v .:? "externalId")
                    )
                        <*> (v .:? "isBaseCurrency")
              )
instance ToJSON Currency where
    toJSON
        ( Currency
                lastModifiedDate
                refName
                exchangeRate
                id
                isAnchorCurrency
                links
                symbol
                locale
                includeInFxRateUpdates
                displaySymbol
                symbolPlacement
                name
                overrideCurrencyFormat
                currencyPrecision
                fxRateUpdateTimezone
                isInactive
                formatSample
                externalId
                isBaseCurrency
            ) =
            object
                [ "lastModifiedDate" .= lastModifiedDate
                , "refName" .= refName
                , "exchangeRate" .= exchangeRate
                , "id" .= id
                , "isAnchorCurrency" .= isAnchorCurrency
                , "links" .= links
                , "symbol" .= symbol
                , "locale" .= locale
                , "includeInFxRateUpdates" .= includeInFxRateUpdates
                , "displaySymbol" .= displaySymbol
                , "symbolPlacement" .= symbolPlacement
                , "name" .= name
                , "overrideCurrencyFormat" .= overrideCurrencyFormat
                , "currencyPrecision" .= currencyPrecision
                , "fxRateUpdateTimezone" .= fxRateUpdateTimezone
                , "isInactive" .= isInactive
                , "formatSample" .= formatSample
                , "externalId" .= externalId
                , "isBaseCurrency" .= isBaseCurrency
                ]
data NsObjOErrorPathDetailOErrorQueryParamOErrorHeaderOErrorUrlOErrorCode
    = NsObjOErrorPathDetailOErrorQueryParamOErrorHeaderOErrorUrlOErrorCode
    { oErrorPath :: (Maybe Text)
    , detail :: (Maybe Text)
    , oErrorQueryParam :: (Maybe Text)
    , oErrorHeader :: (Maybe Text)
    , oErrorUrl :: (Maybe Text)
    , oErrorCode :: (Maybe Text)
    }
    deriving (Show, Eq, Generic)
data NsError
    = NsError
    { status :: (Maybe Int32)
    , type_ :: (Maybe Text)
    , oErrorDetails :: (Vector NsObjOErrorPathDetailOErrorQueryParamOErrorHeaderOErrorUrlOErrorCode)
    , title :: (Maybe Text)
    }
    deriving (Show, Eq, Generic)
instance FromJSON NsObjOErrorPathDetailOErrorQueryParamOErrorHeaderOErrorUrlOErrorCode where
    parseJSON =
        withObject
            "NsObjOErrorPathDetailOErrorQueryParamOErrorHeaderOErrorUrlOErrorCode"
            $ ( \v ->
                    ( ( ( ( ( NsObjOErrorPathDetailOErrorQueryParamOErrorHeaderOErrorUrlOErrorCode
                                <$> (v .:? "o-errorPath")
                            )
                                <*> (v .:? "detail")
                          )
                            <*> (v .:? "o-errorQueryParam")
                        )
                            <*> (v .:? "o-errorHeader")
                      )
                        <*> (v .:? "o-errorUrl")
                    )
                        <*> (v .:? "o-errorCode")
              )
instance ToJSON NsObjOErrorPathDetailOErrorQueryParamOErrorHeaderOErrorUrlOErrorCode where
    toJSON
        ( NsObjOErrorPathDetailOErrorQueryParamOErrorHeaderOErrorUrlOErrorCode
                oErrorPath
                detail
                oErrorQueryParam
                oErrorHeader
                oErrorUrl
                oErrorCode
            ) =
            object
                [ "o-errorPath" .= oErrorPath
                , "detail" .= detail
                , "o-errorQueryParam" .= oErrorQueryParam
                , "o-errorHeader" .= oErrorHeader
                , "o-errorUrl" .= oErrorUrl
                , "o-errorCode" .= oErrorCode
                ]
instance FromJSON NsError where
    parseJSON =
        withObject "NsError"
            $ ( \v ->
                    ( ((NsError <$> (v .:? "status")) <*> (v .:? "type"))
                        <*> ((v .:? "o-errorDetails") .!= V.empty)
                    )
                        <*> (v .:? "title")
              )
instance ToJSON NsError where
    toJSON (NsError status type_ oErrorDetails title) =
        object
            [ "status" .= status
            , "type" .= type_
            , "o-errorDetails" .= oErrorDetails
            , "title" .= title
            ]
