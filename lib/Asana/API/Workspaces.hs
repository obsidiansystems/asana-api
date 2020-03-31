{-
   Asana

   This is the interface for interacting with the [Asana Platform](https://developers.asana.com). Our API reference is generated from our [OpenAPI spec] (https://raw.githubusercontent.com/Asana/developer-docs/master/defs/asana_oas.yaml).

   OpenAPI Version: 3.0.0
   Asana API version: 1.0
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Asana.API.Workspaces
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Asana.API.Workspaces where

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


-- ** Workspaces

-- *** addUserForWorkspace

-- | @POST \/workspaces\/{workspace_gid}\/addUser@
-- 
-- Add a user to a workspace or organization
-- 
-- Add a user to a workspace or organization. The user can be referenced by their globally unique user ID or their email address. Returns the full user record for the invited user.
-- 
-- AuthMethod: 'AuthOAuthOauth2', 'AuthBasicPersonalAccessToken'
-- 
addUserForWorkspace 
  :: (Consumes AddUserForWorkspace MimeJSON, MimeRender MimeJSON InlineObject54)
  => InlineObject54 -- ^ "inlineObject54"
  -> WorkspaceGid -- ^ "workspaceGid" -  Globally unique identifier for the workspace or organization.
  -> AsanaRequest AddUserForWorkspace MimeJSON InlineResponse20028 MimeJSON
addUserForWorkspace inlineObject54 (WorkspaceGid workspaceGid) =
  _mkRequest "POST" ["/workspaces/",toPath workspaceGid,"/addUser"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthOauth2)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicPersonalAccessToken)
    `setBodyParam` inlineObject54

data AddUserForWorkspace 
instance HasBodyParam AddUserForWorkspace InlineObject54 

-- | /Optional Param/ "opt_pretty" - Provides “pretty” output. Provides the response in a “pretty” format. In the case of JSON this means doing proper line breaking and indentation to make it readable. This will take extra time and increase the response size so it is advisable only to use this during debugging.
instance HasOptionalParam AddUserForWorkspace OptPretty where
  applyOptionalParam req (OptPretty xs) =
    req `setQuery` toQuery ("opt_pretty", Just xs)

-- | /Optional Param/ "opt_fields" - Defines fields to return. Some requests return *compact* representations of objects in order to conserve resources and complete the request more efficiently. Other times requests return more information than you may need. This option allows you to list the exact set of fields that the API should be sure to return for the objects. The field names should be provided as paths, described below. The id of included objects will always be returned, regardless of the field options.
instance HasOptionalParam AddUserForWorkspace OptFields where
  applyOptionalParam req (OptFields xs) =
    req `setQuery` toQueryColl CommaSeparated ("opt_fields", Just xs)

-- | @application/json@
instance Consumes AddUserForWorkspace MimeJSON

-- | @application/json@
instance Produces AddUserForWorkspace MimeJSON


-- *** getWorkspace

-- | @GET \/workspaces\/{workspace_gid}@
-- 
-- Get a workspace
-- 
-- Returns the full workspace record for a single workspace.
-- 
-- AuthMethod: 'AuthOAuthOauth2', 'AuthBasicPersonalAccessToken'
-- 
getWorkspace 
  :: WorkspaceGid -- ^ "workspaceGid" -  Globally unique identifier for the workspace or organization.
  -> AsanaRequest GetWorkspace MimeNoContent InlineResponse20037 MimeJSON
getWorkspace (WorkspaceGid workspaceGid) =
  _mkRequest "GET" ["/workspaces/",toPath workspaceGid]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthOauth2)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicPersonalAccessToken)

data GetWorkspace  

-- | /Optional Param/ "opt_pretty" - Provides “pretty” output. Provides the response in a “pretty” format. In the case of JSON this means doing proper line breaking and indentation to make it readable. This will take extra time and increase the response size so it is advisable only to use this during debugging.
instance HasOptionalParam GetWorkspace OptPretty where
  applyOptionalParam req (OptPretty xs) =
    req `setQuery` toQuery ("opt_pretty", Just xs)

-- | /Optional Param/ "opt_fields" - Defines fields to return. Some requests return *compact* representations of objects in order to conserve resources and complete the request more efficiently. Other times requests return more information than you may need. This option allows you to list the exact set of fields that the API should be sure to return for the objects. The field names should be provided as paths, described below. The id of included objects will always be returned, regardless of the field options.
instance HasOptionalParam GetWorkspace OptFields where
  applyOptionalParam req (OptFields xs) =
    req `setQuery` toQueryColl CommaSeparated ("opt_fields", Just xs)
-- | @application/json@
instance Produces GetWorkspace MimeJSON


-- *** getWorkspaces

-- | @GET \/workspaces@
-- 
-- Get multiple workspaces
-- 
-- Returns the compact records for all workspaces visible to the authorized user.
-- 
-- AuthMethod: 'AuthOAuthOauth2', 'AuthBasicPersonalAccessToken'
-- 
getWorkspaces 
  :: AsanaRequest GetWorkspaces MimeNoContent InlineResponse20036 MimeJSON
getWorkspaces =
  _mkRequest "GET" ["/workspaces"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthOauth2)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicPersonalAccessToken)

data GetWorkspaces  

-- | /Optional Param/ "opt_pretty" - Provides “pretty” output. Provides the response in a “pretty” format. In the case of JSON this means doing proper line breaking and indentation to make it readable. This will take extra time and increase the response size so it is advisable only to use this during debugging.
instance HasOptionalParam GetWorkspaces OptPretty where
  applyOptionalParam req (OptPretty xs) =
    req `setQuery` toQuery ("opt_pretty", Just xs)

-- | /Optional Param/ "opt_fields" - Defines fields to return. Some requests return *compact* representations of objects in order to conserve resources and complete the request more efficiently. Other times requests return more information than you may need. This option allows you to list the exact set of fields that the API should be sure to return for the objects. The field names should be provided as paths, described below. The id of included objects will always be returned, regardless of the field options.
instance HasOptionalParam GetWorkspaces OptFields where
  applyOptionalParam req (OptFields xs) =
    req `setQuery` toQueryColl CommaSeparated ("opt_fields", Just xs)

-- | /Optional Param/ "limit" - Results per page. The number of objects to return per page. The value must be between 1 and 100.
instance HasOptionalParam GetWorkspaces Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "offset" - Offset token. An offset to the next page returned by the API. A pagination request will return an offset token, which can be used as an input parameter to the next request. If an offset is not passed in, the API will return the first page of results. 'Note: You can only pass in an offset that was returned to you via a previously paginated request.'
instance HasOptionalParam GetWorkspaces Offset where
  applyOptionalParam req (Offset xs) =
    req `setQuery` toQuery ("offset", Just xs)
-- | @application/json@
instance Produces GetWorkspaces MimeJSON


-- *** removeUserForWorkspace

-- | @POST \/workspaces\/{workspace_gid}\/removeUser@
-- 
-- Remove a user from a workspace or organization
-- 
-- Remove a user from a workspace or organization. The user making this call must be an admin in the workspace. The user can be referenced by their globally unique user ID or their email address. Returns an empty data record.
-- 
-- AuthMethod: 'AuthOAuthOauth2', 'AuthBasicPersonalAccessToken'
-- 
removeUserForWorkspace 
  :: (Consumes RemoveUserForWorkspace MimeJSON, MimeRender MimeJSON InlineObject55)
  => InlineObject55 -- ^ "inlineObject55"
  -> WorkspaceGid -- ^ "workspaceGid" -  Globally unique identifier for the workspace or organization.
  -> AsanaRequest RemoveUserForWorkspace MimeJSON InlineResponse2001 MimeJSON
removeUserForWorkspace inlineObject55 (WorkspaceGid workspaceGid) =
  _mkRequest "POST" ["/workspaces/",toPath workspaceGid,"/removeUser"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthOauth2)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicPersonalAccessToken)
    `setBodyParam` inlineObject55

data RemoveUserForWorkspace 
instance HasBodyParam RemoveUserForWorkspace InlineObject55 

-- | /Optional Param/ "opt_pretty" - Provides “pretty” output. Provides the response in a “pretty” format. In the case of JSON this means doing proper line breaking and indentation to make it readable. This will take extra time and increase the response size so it is advisable only to use this during debugging.
instance HasOptionalParam RemoveUserForWorkspace OptPretty where
  applyOptionalParam req (OptPretty xs) =
    req `setQuery` toQuery ("opt_pretty", Just xs)

-- | /Optional Param/ "opt_fields" - Defines fields to return. Some requests return *compact* representations of objects in order to conserve resources and complete the request more efficiently. Other times requests return more information than you may need. This option allows you to list the exact set of fields that the API should be sure to return for the objects. The field names should be provided as paths, described below. The id of included objects will always be returned, regardless of the field options.
instance HasOptionalParam RemoveUserForWorkspace OptFields where
  applyOptionalParam req (OptFields xs) =
    req `setQuery` toQueryColl CommaSeparated ("opt_fields", Just xs)

-- | @application/json@
instance Consumes RemoveUserForWorkspace MimeJSON

-- | @application/json@
instance Produces RemoveUserForWorkspace MimeJSON


-- *** updateWorkspace

-- | @PUT \/workspaces\/{workspace_gid}@
-- 
-- Update a workspace
-- 
-- A specific, existing workspace can be updated by making a PUT request on the URL for that workspace. Only the fields provided in the data block will be updated; any unspecified fields will remain unchanged. Currently the only field that can be modified for a workspace is its name. Returns the complete, updated workspace record.
-- 
-- AuthMethod: 'AuthOAuthOauth2', 'AuthBasicPersonalAccessToken'
-- 
updateWorkspace 
  :: (Consumes UpdateWorkspace MimeJSON, MimeRender MimeJSON InlineObject53)
  => InlineObject53 -- ^ "inlineObject53"
  -> WorkspaceGid -- ^ "workspaceGid" -  Globally unique identifier for the workspace or organization.
  -> AsanaRequest UpdateWorkspace MimeJSON InlineResponse20037 MimeJSON
updateWorkspace inlineObject53 (WorkspaceGid workspaceGid) =
  _mkRequest "PUT" ["/workspaces/",toPath workspaceGid]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthOauth2)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicPersonalAccessToken)
    `setBodyParam` inlineObject53

data UpdateWorkspace 
instance HasBodyParam UpdateWorkspace InlineObject53 

-- | /Optional Param/ "opt_pretty" - Provides “pretty” output. Provides the response in a “pretty” format. In the case of JSON this means doing proper line breaking and indentation to make it readable. This will take extra time and increase the response size so it is advisable only to use this during debugging.
instance HasOptionalParam UpdateWorkspace OptPretty where
  applyOptionalParam req (OptPretty xs) =
    req `setQuery` toQuery ("opt_pretty", Just xs)

-- | /Optional Param/ "opt_fields" - Defines fields to return. Some requests return *compact* representations of objects in order to conserve resources and complete the request more efficiently. Other times requests return more information than you may need. This option allows you to list the exact set of fields that the API should be sure to return for the objects. The field names should be provided as paths, described below. The id of included objects will always be returned, regardless of the field options.
instance HasOptionalParam UpdateWorkspace OptFields where
  applyOptionalParam req (OptFields xs) =
    req `setQuery` toQueryColl CommaSeparated ("opt_fields", Just xs)

-- | @application/json@
instance Consumes UpdateWorkspace MimeJSON

-- | @application/json@
instance Produces UpdateWorkspace MimeJSON

