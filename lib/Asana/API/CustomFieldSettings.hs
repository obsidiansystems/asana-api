{-
   Asana

   This is the interface for interacting with the [Asana Platform](https://developers.asana.com). Our API reference is generated from our [OpenAPI spec] (https://raw.githubusercontent.com/Asana/developer-docs/master/defs/asana_oas.yaml).

   OpenAPI Version: 3.0.0
   Asana API version: 1.0
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Asana.API.CustomFieldSettings
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Asana.API.CustomFieldSettings where

import Asana.Core
import Asana.MimeTypes
import Asana.Model as M

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Set as Set
import qualified Data.String as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as TI
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Data.Text (Text)
import GHC.Base ((<|>))

import Prelude ((==),(/=),($), (.),(<$>),(<*>),(>>=),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P

-- * Operations


-- ** CustomFieldSettings

-- *** getCustomFieldSettingsForPortfolio

-- | @GET \/portfolios\/{portfolio_gid}\/custom_field_settings@
-- 
-- Get a portfolio's custom fields
-- 
-- Returns a list of all of the custom fields settings on a portfolio, in compact form.
-- 
-- AuthMethod: 'AuthOAuthOauth2', 'AuthBasicPersonalAccessToken'
-- 
getCustomFieldSettingsForPortfolio 
  :: PortfolioGid -- ^ "portfolioGid" -  Globally unique identifier for the portfolio.
  -> AsanaRequest GetCustomFieldSettingsForPortfolio MimeNoContent InlineResponse2004 MimeJSON
getCustomFieldSettingsForPortfolio (PortfolioGid portfolioGid) =
  _mkRequest "GET" ["/portfolios/",toPath portfolioGid,"/custom_field_settings"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthOauth2)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicPersonalAccessToken)

data GetCustomFieldSettingsForPortfolio  

-- | /Optional Param/ "opt_pretty" - Provides “pretty” output. Provides the response in a “pretty” format. In the case of JSON this means doing proper line breaking and indentation to make it readable. This will take extra time and increase the response size so it is advisable only to use this during debugging.
instance HasOptionalParam GetCustomFieldSettingsForPortfolio OptPretty where
  applyOptionalParam req (OptPretty xs) =
    req `setQuery` toQuery ("opt_pretty", Just xs)

-- | /Optional Param/ "opt_fields" - Defines fields to return. Some requests return *compact* representations of objects in order to conserve resources and complete the request more efficiently. Other times requests return more information than you may need. This option allows you to list the exact set of fields that the API should be sure to return for the objects. The field names should be provided as paths, described below. The id of included objects will always be returned, regardless of the field options.
instance HasOptionalParam GetCustomFieldSettingsForPortfolio OptFields where
  applyOptionalParam req (OptFields xs) =
    req `setQuery` toQueryColl CommaSeparated ("opt_fields", Just xs)

-- | /Optional Param/ "limit" - Results per page. The number of objects to return per page. The value must be between 1 and 100.
instance HasOptionalParam GetCustomFieldSettingsForPortfolio Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "offset" - Offset token. An offset to the next page returned by the API. A pagination request will return an offset token, which can be used as an input parameter to the next request. If an offset is not passed in, the API will return the first page of results. 'Note: You can only pass in an offset that was returned to you via a previously paginated request.'
instance HasOptionalParam GetCustomFieldSettingsForPortfolio Offset where
  applyOptionalParam req (Offset xs) =
    req `setQuery` toQuery ("offset", Just xs)
-- | @application/json@
instance Produces GetCustomFieldSettingsForPortfolio MimeJSON


-- *** getCustomFieldSettingsForProject

-- | @GET \/projects\/{project_gid}\/custom_field_settings@
-- 
-- Get a project's custom fields
-- 
-- Returns a list of all of the custom fields settings on a project, in compact form. Note that, as in all queries to collections which return compact representation, `opt_fields` can be used to include more data than is returned in the compact representation. See the [getting started guide on input/output options](https://developers.asana.com/docs/#input-output-options) for more information.
-- 
-- AuthMethod: 'AuthOAuthOauth2', 'AuthBasicPersonalAccessToken'
-- 
getCustomFieldSettingsForProject 
  :: ProjectGid -- ^ "projectGid" -  Globally unique identifier for the project.
  -> AsanaRequest GetCustomFieldSettingsForProject MimeNoContent InlineResponse2004 MimeJSON
getCustomFieldSettingsForProject (ProjectGid projectGid) =
  _mkRequest "GET" ["/projects/",toPath projectGid,"/custom_field_settings"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthOauth2)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicPersonalAccessToken)

data GetCustomFieldSettingsForProject  

-- | /Optional Param/ "opt_pretty" - Provides “pretty” output. Provides the response in a “pretty” format. In the case of JSON this means doing proper line breaking and indentation to make it readable. This will take extra time and increase the response size so it is advisable only to use this during debugging.
instance HasOptionalParam GetCustomFieldSettingsForProject OptPretty where
  applyOptionalParam req (OptPretty xs) =
    req `setQuery` toQuery ("opt_pretty", Just xs)

-- | /Optional Param/ "opt_fields" - Defines fields to return. Some requests return *compact* representations of objects in order to conserve resources and complete the request more efficiently. Other times requests return more information than you may need. This option allows you to list the exact set of fields that the API should be sure to return for the objects. The field names should be provided as paths, described below. The id of included objects will always be returned, regardless of the field options.
instance HasOptionalParam GetCustomFieldSettingsForProject OptFields where
  applyOptionalParam req (OptFields xs) =
    req `setQuery` toQueryColl CommaSeparated ("opt_fields", Just xs)

-- | /Optional Param/ "limit" - Results per page. The number of objects to return per page. The value must be between 1 and 100.
instance HasOptionalParam GetCustomFieldSettingsForProject Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "offset" - Offset token. An offset to the next page returned by the API. A pagination request will return an offset token, which can be used as an input parameter to the next request. If an offset is not passed in, the API will return the first page of results. 'Note: You can only pass in an offset that was returned to you via a previously paginated request.'
instance HasOptionalParam GetCustomFieldSettingsForProject Offset where
  applyOptionalParam req (Offset xs) =
    req `setQuery` toQuery ("offset", Just xs)
-- | @application/json@
instance Produces GetCustomFieldSettingsForProject MimeJSON

