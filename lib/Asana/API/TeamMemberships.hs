{-
   Asana

   This is the interface for interacting with the [Asana Platform](https://developers.asana.com). Our API reference is generated from our [OpenAPI spec] (https://raw.githubusercontent.com/Asana/developer-docs/master/defs/asana_oas.yaml).

   OpenAPI Version: 3.0.0
   Asana API version: 1.0
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Asana.API.TeamMemberships
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Asana.API.TeamMemberships where

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


-- ** TeamMemberships

-- *** getTeamMembership

-- | @GET \/team_memberships\/{team_membership_gid}@
-- 
-- Get a team membership
-- 
-- Returns the complete team membership record for a single team membership.
-- 
-- AuthMethod: 'AuthOAuthOauth2', 'AuthBasicPersonalAccessToken'
-- 
getTeamMembership 
  :: TeamMembershipGid -- ^ "teamMembershipGid"
  -> AsanaRequest GetTeamMembership MimeNoContent InlineResponse20024 MimeJSON
getTeamMembership (TeamMembershipGid teamMembershipGid) =
  _mkRequest "GET" ["/team_memberships/",toPath teamMembershipGid]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthOauth2)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicPersonalAccessToken)

data GetTeamMembership  

-- | /Optional Param/ "opt_pretty" - Provides “pretty” output. Provides the response in a “pretty” format. In the case of JSON this means doing proper line breaking and indentation to make it readable. This will take extra time and increase the response size so it is advisable only to use this during debugging.
instance HasOptionalParam GetTeamMembership OptPretty where
  applyOptionalParam req (OptPretty xs) =
    req `setQuery` toQuery ("opt_pretty", Just xs)

-- | /Optional Param/ "opt_fields" - Defines fields to return. Some requests return *compact* representations of objects in order to conserve resources and complete the request more efficiently. Other times requests return more information than you may need. This option allows you to list the exact set of fields that the API should be sure to return for the objects. The field names should be provided as paths, described below. The id of included objects will always be returned, regardless of the field options.
instance HasOptionalParam GetTeamMembership OptFields where
  applyOptionalParam req (OptFields xs) =
    req `setQuery` toQueryColl CommaSeparated ("opt_fields", Just xs)
-- | @application/json@
instance Produces GetTeamMembership MimeJSON


-- *** getTeamMemberships

-- | @GET \/team_memberships@
-- 
-- Get team memberships
-- 
-- Returns compact team membership records.
-- 
-- AuthMethod: 'AuthOAuthOauth2', 'AuthBasicPersonalAccessToken'
-- 
getTeamMemberships 
  :: AsanaRequest GetTeamMemberships MimeNoContent InlineResponse20025 MimeJSON
getTeamMemberships =
  _mkRequest "GET" ["/team_memberships"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthOauth2)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicPersonalAccessToken)

data GetTeamMemberships  

-- | /Optional Param/ "opt_pretty" - Provides “pretty” output. Provides the response in a “pretty” format. In the case of JSON this means doing proper line breaking and indentation to make it readable. This will take extra time and increase the response size so it is advisable only to use this during debugging.
instance HasOptionalParam GetTeamMemberships OptPretty where
  applyOptionalParam req (OptPretty xs) =
    req `setQuery` toQuery ("opt_pretty", Just xs)

-- | /Optional Param/ "opt_fields" - Defines fields to return. Some requests return *compact* representations of objects in order to conserve resources and complete the request more efficiently. Other times requests return more information than you may need. This option allows you to list the exact set of fields that the API should be sure to return for the objects. The field names should be provided as paths, described below. The id of included objects will always be returned, regardless of the field options.
instance HasOptionalParam GetTeamMemberships OptFields where
  applyOptionalParam req (OptFields xs) =
    req `setQuery` toQueryColl CommaSeparated ("opt_fields", Just xs)

-- | /Optional Param/ "limit" - Results per page. The number of objects to return per page. The value must be between 1 and 100.
instance HasOptionalParam GetTeamMemberships Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "offset" - Offset token. An offset to the next page returned by the API. A pagination request will return an offset token, which can be used as an input parameter to the next request. If an offset is not passed in, the API will return the first page of results. 'Note: You can only pass in an offset that was returned to you via a previously paginated request.'
instance HasOptionalParam GetTeamMemberships Offset where
  applyOptionalParam req (Offset xs) =
    req `setQuery` toQuery ("offset", Just xs)

-- | /Optional Param/ "team" - Globally unique identifier for the team.
instance HasOptionalParam GetTeamMemberships Team where
  applyOptionalParam req (Team xs) =
    req `setQuery` toQuery ("team", Just xs)

-- | /Optional Param/ "user" - A string identifying a user. This can either be the string \"me\", an email, or the gid of a user. This parameter must be used with the workspace parameter.
instance HasOptionalParam GetTeamMemberships User where
  applyOptionalParam req (User xs) =
    req `setQuery` toQuery ("user", Just xs)

-- | /Optional Param/ "workspace" - Globally unique identifier for the workspace. This parameter must be used with the user parameter.
instance HasOptionalParam GetTeamMemberships Workspace where
  applyOptionalParam req (Workspace xs) =
    req `setQuery` toQuery ("workspace", Just xs)
-- | @application/json@
instance Produces GetTeamMemberships MimeJSON


-- *** getTeamMembershipsForTeam

-- | @GET \/teams\/{team_gid}\/team_memberships@
-- 
-- Get memberships from a team
-- 
-- Returns the compact team memberships for the team.
-- 
-- AuthMethod: 'AuthOAuthOauth2', 'AuthBasicPersonalAccessToken'
-- 
getTeamMembershipsForTeam 
  :: TeamGid -- ^ "teamGid" -  Globally unique identifier for the team.
  -> AsanaRequest GetTeamMembershipsForTeam MimeNoContent InlineResponse20025 MimeJSON
getTeamMembershipsForTeam (TeamGid teamGid) =
  _mkRequest "GET" ["/teams/",toPath teamGid,"/team_memberships"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthOauth2)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicPersonalAccessToken)

data GetTeamMembershipsForTeam  

-- | /Optional Param/ "opt_pretty" - Provides “pretty” output. Provides the response in a “pretty” format. In the case of JSON this means doing proper line breaking and indentation to make it readable. This will take extra time and increase the response size so it is advisable only to use this during debugging.
instance HasOptionalParam GetTeamMembershipsForTeam OptPretty where
  applyOptionalParam req (OptPretty xs) =
    req `setQuery` toQuery ("opt_pretty", Just xs)

-- | /Optional Param/ "opt_fields" - Defines fields to return. Some requests return *compact* representations of objects in order to conserve resources and complete the request more efficiently. Other times requests return more information than you may need. This option allows you to list the exact set of fields that the API should be sure to return for the objects. The field names should be provided as paths, described below. The id of included objects will always be returned, regardless of the field options.
instance HasOptionalParam GetTeamMembershipsForTeam OptFields where
  applyOptionalParam req (OptFields xs) =
    req `setQuery` toQueryColl CommaSeparated ("opt_fields", Just xs)

-- | /Optional Param/ "limit" - Results per page. The number of objects to return per page. The value must be between 1 and 100.
instance HasOptionalParam GetTeamMembershipsForTeam Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "offset" - Offset token. An offset to the next page returned by the API. A pagination request will return an offset token, which can be used as an input parameter to the next request. If an offset is not passed in, the API will return the first page of results. 'Note: You can only pass in an offset that was returned to you via a previously paginated request.'
instance HasOptionalParam GetTeamMembershipsForTeam Offset where
  applyOptionalParam req (Offset xs) =
    req `setQuery` toQuery ("offset", Just xs)
-- | @application/json@
instance Produces GetTeamMembershipsForTeam MimeJSON


-- *** getTeamMembershipsForUser

-- | @GET \/users\/{user_gid}\/team_memberships@
-- 
-- Get memberships from a user
-- 
-- Returns the compact team membership records for the user.
-- 
-- AuthMethod: 'AuthOAuthOauth2', 'AuthBasicPersonalAccessToken'
-- 
getTeamMembershipsForUser 
  :: UserGid -- ^ "userGid" -  A string identifying a user. This can either be the string \"me\", an email, or the gid of a user.
  -> Workspace -- ^ "workspace" -  Globally unique identifier for the workspace.
  -> AsanaRequest GetTeamMembershipsForUser MimeNoContent InlineResponse20025 MimeJSON
getTeamMembershipsForUser (UserGid userGid) (Workspace workspace) =
  _mkRequest "GET" ["/users/",toPath userGid,"/team_memberships"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthOauth2)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicPersonalAccessToken)
    `setQuery` toQuery ("workspace", Just workspace)

data GetTeamMembershipsForUser  

-- | /Optional Param/ "opt_pretty" - Provides “pretty” output. Provides the response in a “pretty” format. In the case of JSON this means doing proper line breaking and indentation to make it readable. This will take extra time and increase the response size so it is advisable only to use this during debugging.
instance HasOptionalParam GetTeamMembershipsForUser OptPretty where
  applyOptionalParam req (OptPretty xs) =
    req `setQuery` toQuery ("opt_pretty", Just xs)

-- | /Optional Param/ "opt_fields" - Defines fields to return. Some requests return *compact* representations of objects in order to conserve resources and complete the request more efficiently. Other times requests return more information than you may need. This option allows you to list the exact set of fields that the API should be sure to return for the objects. The field names should be provided as paths, described below. The id of included objects will always be returned, regardless of the field options.
instance HasOptionalParam GetTeamMembershipsForUser OptFields where
  applyOptionalParam req (OptFields xs) =
    req `setQuery` toQueryColl CommaSeparated ("opt_fields", Just xs)

-- | /Optional Param/ "limit" - Results per page. The number of objects to return per page. The value must be between 1 and 100.
instance HasOptionalParam GetTeamMembershipsForUser Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "offset" - Offset token. An offset to the next page returned by the API. A pagination request will return an offset token, which can be used as an input parameter to the next request. If an offset is not passed in, the API will return the first page of results. 'Note: You can only pass in an offset that was returned to you via a previously paginated request.'
instance HasOptionalParam GetTeamMembershipsForUser Offset where
  applyOptionalParam req (Offset xs) =
    req `setQuery` toQuery ("offset", Just xs)
-- | @application/json@
instance Produces GetTeamMembershipsForUser MimeJSON

