-- | 

{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE OverloadedStrings         #-}


module WebApi.Schema where

import Data.Proxy
import WebApi.Docs
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Data.Text (Text)

discovery :: Proxy api -> ()
discovery = undefined
