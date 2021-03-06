{-
   Asana

   This is the interface for interacting with the [Asana Platform](https://developers.asana.com). Our API reference is generated from our [OpenAPI spec] (https://raw.githubusercontent.com/Asana/developer-docs/master/defs/asana_oas.yaml).

   OpenAPI Version: 3.0.0
   Asana API version: 1.0
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Asana.API.Teams
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Asana.API.Teams where

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


-- ** Teams

-- *** addUserForTeam

-- | @POST \/teams\/{team_gid}\/addUser@
-- 
-- Add a user to a team
-- 
-- The user making this call must be a member of the team in order to add others. The user being added must exist in the same organization as the team.
-- 
-- AuthMethod: 'AuthOAuthOauth2', 'AuthBasicPersonalAccessToken'
-- 
addUserForTeam 
  :: (Consumes AddUserForTeam MimeJSON, MimeRender MimeJSON InlineObject50)
  => InlineObject50 -- ^ "inlineObject50"
  -> TeamGid -- ^ "teamGid" -  Globally unique identifier for the team.
  -> AsanaRequest AddUserForTeam MimeJSON InlineResponse20028 MimeJSON
addUserForTeam inlineObject50 (TeamGid teamGid) =
  _mkRequest "POST" ["/teams/",toPath teamGid,"/addUser"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthOauth2)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicPersonalAccessToken)
    `setBodyParam` inlineObject50

data AddUserForTeam 
instance HasBodyParam AddUserForTeam InlineObject50 

-- | /Optional Param/ "opt_pretty" - Provides “pretty” output. Provides the response in a “pretty” format. In the case of JSON this means doing proper line breaking and indentation to make it readable. This will take extra time and increase the response size so it is advisable only to use this during debugging.
instance HasOptionalParam AddUserForTeam OptPretty where
  applyOptionalParam req (OptPretty xs) =
    req `setQuery` toQuery ("opt_pretty", Just xs)

-- | /Optional Param/ "opt_fields" - Defines fields to return. Some requests return *compact* representations of objects in order to conserve resources and complete the request more efficiently. Other times requests return more information than you may need. This option allows you to list the exact set of fields that the API should be sure to return for the objects. The field names should be provided as paths, described below. The id of included objects will always be returned, regardless of the field options.
instance HasOptionalParam AddUserForTeam OptFields where
  applyOptionalParam req (OptFields xs) =
    req `setQuery` toQueryColl CommaSeparated ("opt_fields", Just xs)

-- | @application/json@
instance Consumes AddUserForTeam MimeJSON

-- | @application/json@
instance Produces AddUserForTeam MimeJSON


-- *** getTeam

-- | @GET \/teams\/{team_gid}@
-- 
-- Get a team
-- 
-- Returns the full record for a single team.
-- 
-- AuthMethod: 'AuthOAuthOauth2', 'AuthBasicPersonalAccessToken'
-- 
getTeam 
  :: TeamGid -- ^ "teamGid" -  Globally unique identifier for the team.
  -> AsanaRequest GetTeam MimeNoContent InlineResponse20026 MimeJSON
getTeam (TeamGid teamGid) =
  _mkRequest "GET" ["/teams/",toPath teamGid]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthOauth2)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicPersonalAccessToken)

data GetTeam  

-- | /Optional Param/ "opt_pretty" - Provides “pretty” output. Provides the response in a “pretty” format. In the case of JSON this means doing proper line breaking and indentation to make it readable. This will take extra time and increase the response size so it is advisable only to use this during debugging.
instance HasOptionalParam GetTeam OptPretty where
  applyOptionalParam req (OptPretty xs) =
    req `setQuery` toQuery ("opt_pretty", Just xs)

-- | /Optional Param/ "opt_fields" - Defines fields to return. Some requests return *compact* representations of objects in order to conserve resources and complete the request more efficiently. Other times requests return more information than you may need. This option allows you to list the exact set of fields that the API should be sure to return for the objects. The field names should be provided as paths, described below. The id of included objects will always be returned, regardless of the field options.
instance HasOptionalParam GetTeam OptFields where
  applyOptionalParam req (OptFields xs) =
    req `setQuery` toQueryColl CommaSeparated ("opt_fields", Just xs)

-- | /Optional Param/ "limit" - Results per page. The number of objects to return per page. The value must be between 1 and 100.
instance HasOptionalParam GetTeam Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "offset" - Offset token. An offset to the next page returned by the API. A pagination request will return an offset token, which can be used as an input parameter to the next request. If an offset is not passed in, the API will return the first page of results. 'Note: You can only pass in an offset that was returned to you via a previously paginated request.'
instance HasOptionalParam GetTeam Offset where
  applyOptionalParam req (Offset xs) =
    req `setQuery` toQuery ("offset", Just xs)
-- | @application/json@
instance Produces GetTeam MimeJSON


-- *** getTeamsForOrganization

-- | @GET \/organizations\/{workspace_gid}\/teams@
-- 
-- Get teams in an organization
-- 
-- Returns the compact records for all teams in the organization visible to the authorized user.
-- 
-- AuthMethod: 'AuthOAuthOauth2', 'AuthBasicPersonalAccessToken'
-- 
getTeamsForOrganization 
  :: WorkspaceGid -- ^ "workspaceGid" -  Globally unique identifier for the workspace or organization.
  -> AsanaRequest GetTeamsForOrganization MimeNoContent InlineResponse20027 MimeJSON
getTeamsForOrganization (WorkspaceGid workspaceGid) =
  _mkRequest "GET" ["/organizations/",toPath workspaceGid,"/teams"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthOauth2)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicPersonalAccessToken)

data GetTeamsForOrganization  

-- | /Optional Param/ "opt_pretty" - Provides “pretty” output. Provides the response in a “pretty” format. In the case of JSON this means doing proper line breaking and indentation to make it readable. This will take extra time and increase the response size so it is advisable only to use this during debugging.
instance HasOptionalParam GetTeamsForOrganization OptPretty where
  applyOptionalParam req (OptPretty xs) =
    req `setQuery` toQuery ("opt_pretty", Just xs)

-- | /Optional Param/ "opt_fields" - Defines fields to return. Some requests return *compact* representations of objects in order to conserve resources and complete the request more efficiently. Other times requests return more information than you may need. This option allows you to list the exact set of fields that the API should be sure to return for the objects. The field names should be provided as paths, described below. The id of included objects will always be returned, regardless of the field options.
instance HasOptionalParam GetTeamsForOrganization OptFields where
  applyOptionalParam req (OptFields xs) =
    req `setQuery` toQueryColl CommaSeparated ("opt_fields", Just xs)

-- | /Optional Param/ "limit" - Results per page. The number of objects to return per page. The value must be between 1 and 100.
instance HasOptionalParam GetTeamsForOrganization Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "offset" - Offset token. An offset to the next page returned by the API. A pagination request will return an offset token, which can be used as an input parameter to the next request. If an offset is not passed in, the API will return the first page of results. 'Note: You can only pass in an offset that was returned to you via a previously paginated request.'
instance HasOptionalParam GetTeamsForOrganization Offset where
  applyOptionalParam req (Offset xs) =
    req `setQuery` toQuery ("offset", Just xs)
-- | @application/json@
instance Produces GetTeamsForOrganization MimeJSON


-- *** getTeamsForUser

-- | @GET \/users\/{user_gid}\/teams@
-- 
-- Get teams for a user
-- 
-- Returns the compact records for all teams to which the given user is assigned.
-- 
-- AuthMethod: 'AuthOAuthOauth2', 'AuthBasicPersonalAccessToken'
-- 
getTeamsForUser 
  :: UserGid -- ^ "userGid" -  A string identifying a user. This can either be the string \"me\", an email, or the gid of a user.
  -> Organization -- ^ "organization" -  The workspace or organization to filter teams on.
  -> AsanaRequest GetTeamsForUser MimeNoContent InlineResponse20027 MimeJSON
getTeamsForUser (UserGid userGid) (Organization organization) =
  _mkRequest "GET" ["/users/",toPath userGid,"/teams"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthOauth2)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicPersonalAccessToken)
    `setQuery` toQuery ("organization", Just organization)

data GetTeamsForUser  

-- | /Optional Param/ "opt_pretty" - Provides “pretty” output. Provides the response in a “pretty” format. In the case of JSON this means doing proper line breaking and indentation to make it readable. This will take extra time and increase the response size so it is advisable only to use this during debugging.
instance HasOptionalParam GetTeamsForUser OptPretty where
  applyOptionalParam req (OptPretty xs) =
    req `setQuery` toQuery ("opt_pretty", Just xs)

-- | /Optional Param/ "opt_fields" - Defines fields to return. Some requests return *compact* representations of objects in order to conserve resources and complete the request more efficiently. Other times requests return more information than you may need. This option allows you to list the exact set of fields that the API should be sure to return for the objects. The field names should be provided as paths, described below. The id of included objects will always be returned, regardless of the field options.
instance HasOptionalParam GetTeamsForUser OptFields where
  applyOptionalParam req (OptFields xs) =
    req `setQuery` toQueryColl CommaSeparated ("opt_fields", Just xs)

-- | /Optional Param/ "limit" - Results per page. The number of objects to return per page. The value must be between 1 and 100.
instance HasOptionalParam GetTeamsForUser Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "offset" - Offset token. An offset to the next page returned by the API. A pagination request will return an offset token, which can be used as an input parameter to the next request. If an offset is not passed in, the API will return the first page of results. 'Note: You can only pass in an offset that was returned to you via a previously paginated request.'
instance HasOptionalParam GetTeamsForUser Offset where
  applyOptionalParam req (Offset xs) =
    req `setQuery` toQuery ("offset", Just xs)
-- | @application/json@
instance Produces GetTeamsForUser MimeJSON


-- *** removeUserForTeam

-- | @POST \/teams\/{team_gid}\/removeUser@
-- 
-- Remove a user from a team
-- 
-- The user making this call must be a member of the team in order to remove themselves or others.
-- 
-- AuthMethod: 'AuthOAuthOauth2', 'AuthBasicPersonalAccessToken'
-- 
removeUserForTeam 
  :: (Consumes RemoveUserForTeam MimeJSON, MimeRender MimeJSON InlineObject51)
  => InlineObject51 -- ^ "inlineObject51"
  -> TeamGid -- ^ "teamGid" -  Globally unique identifier for the team.
  -> AsanaRequest RemoveUserForTeam MimeJSON InlineResponse2001 MimeJSON
removeUserForTeam inlineObject51 (TeamGid teamGid) =
  _mkRequest "POST" ["/teams/",toPath teamGid,"/removeUser"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthOauth2)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicPersonalAccessToken)
    `setBodyParam` inlineObject51

data RemoveUserForTeam 
instance HasBodyParam RemoveUserForTeam InlineObject51 

-- | /Optional Param/ "opt_pretty" - Provides “pretty” output. Provides the response in a “pretty” format. In the case of JSON this means doing proper line breaking and indentation to make it readable. This will take extra time and increase the response size so it is advisable only to use this during debugging.
instance HasOptionalParam RemoveUserForTeam OptPretty where
  applyOptionalParam req (OptPretty xs) =
    req `setQuery` toQuery ("opt_pretty", Just xs)

-- | /Optional Param/ "opt_fields" - Defines fields to return. Some requests return *compact* representations of objects in order to conserve resources and complete the request more efficiently. Other times requests return more information than you may need. This option allows you to list the exact set of fields that the API should be sure to return for the objects. The field names should be provided as paths, described below. The id of included objects will always be returned, regardless of the field options.
instance HasOptionalParam RemoveUserForTeam OptFields where
  applyOptionalParam req (OptFields xs) =
    req `setQuery` toQueryColl CommaSeparated ("opt_fields", Just xs)

-- | @application/json@
instance Consumes RemoveUserForTeam MimeJSON

-- | @application/json@
instance Produces RemoveUserForTeam MimeJSON

