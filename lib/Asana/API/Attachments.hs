{-
   Asana

   This is the interface for interacting with the [Asana Platform](https://developers.asana.com). Our API reference is generated from our [OpenAPI spec] (https://raw.githubusercontent.com/Asana/developer-docs/master/defs/asana_oas.yaml).

   OpenAPI Version: 3.0.0
   Asana API version: 1.0
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Asana.API.Attachments
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Asana.API.Attachments where

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


-- ** Attachments

-- *** createAttachmentForTask

-- | @POST \/tasks\/{task_gid}\/attachments@
-- 
-- Upload an attachment
-- 
-- Upload an attachment.  This method uploads an attachment to a task and returns the compact record for the created attachment object. It is not possible to attach files from third party services such as Dropbox, Box & Google Drive via the API. You must download the file content first and then upload it as any other attachment.  The 100MB size limit on attachments in Asana is enforced on this endpoint.  This endpoint expects a multipart/form-data encoded request containing the full contents of the file to be uploaded.  Requests made should follow the HTTP/1.1 specification that line terminators are of the form `CRLF` or `\\r\\n` outlined [here](http://www.w3.org/Protocols/HTTP/1.1/draft-ietf-http-v11-spec-01#Basic-Rules) in order for the server to reliably and properly handle the request.
-- 
-- AuthMethod: 'AuthOAuthOauth2', 'AuthBasicPersonalAccessToken'
-- 
createAttachmentForTask 
  :: (Consumes CreateAttachmentForTask MimeMultipartFormData)
  => TaskGid -- ^ "taskGid" -  The task to operate on.
  -> AsanaRequest CreateAttachmentForTask MimeMultipartFormData InlineResponse200 MimeJSON
createAttachmentForTask (TaskGid taskGid) =
  _mkRequest "POST" ["/tasks/",toPath taskGid,"/attachments"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthOauth2)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicPersonalAccessToken)

data CreateAttachmentForTask  
instance HasOptionalParam CreateAttachmentForTask File where
  applyOptionalParam req (File xs) =
    req `_addMultiFormPart` NH.partFileSource "file" xs

-- | /Optional Param/ "opt_pretty" - Provides “pretty” output. Provides the response in a “pretty” format. In the case of JSON this means doing proper line breaking and indentation to make it readable. This will take extra time and increase the response size so it is advisable only to use this during debugging.
instance HasOptionalParam CreateAttachmentForTask OptPretty where
  applyOptionalParam req (OptPretty xs) =
    req `setQuery` toQuery ("opt_pretty", Just xs)

-- | /Optional Param/ "opt_fields" - Defines fields to return. Some requests return *compact* representations of objects in order to conserve resources and complete the request more efficiently. Other times requests return more information than you may need. This option allows you to list the exact set of fields that the API should be sure to return for the objects. The field names should be provided as paths, described below. The id of included objects will always be returned, regardless of the field options.
instance HasOptionalParam CreateAttachmentForTask OptFields where
  applyOptionalParam req (OptFields xs) =
    req `setQuery` toQueryColl CommaSeparated ("opt_fields", Just xs)

-- | /Optional Param/ "limit" - Results per page. The number of objects to return per page. The value must be between 1 and 100.
instance HasOptionalParam CreateAttachmentForTask Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "offset" - Offset token. An offset to the next page returned by the API. A pagination request will return an offset token, which can be used as an input parameter to the next request. If an offset is not passed in, the API will return the first page of results. 'Note: You can only pass in an offset that was returned to you via a previously paginated request.'
instance HasOptionalParam CreateAttachmentForTask Offset where
  applyOptionalParam req (Offset xs) =
    req `setQuery` toQuery ("offset", Just xs)

-- | @multipart/form-data@
instance Consumes CreateAttachmentForTask MimeMultipartFormData

-- | @application/json@
instance Produces CreateAttachmentForTask MimeJSON


-- *** deleteAttachment

-- | @DELETE \/attachments\/{attachment_gid}@
-- 
-- Delete an attachment
-- 
-- Deletes a specific, existing attachment.  Returns an empty data record.
-- 
-- AuthMethod: 'AuthOAuthOauth2', 'AuthBasicPersonalAccessToken'
-- 
deleteAttachment 
  :: AttachmentGid -- ^ "attachmentGid" -  Globally unique identifier for the attachment.
  -> AsanaRequest DeleteAttachment MimeNoContent InlineResponse2001 MimeJSON
deleteAttachment (AttachmentGid attachmentGid) =
  _mkRequest "DELETE" ["/attachments/",toPath attachmentGid]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthOauth2)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicPersonalAccessToken)

data DeleteAttachment  

-- | /Optional Param/ "opt_pretty" - Provides “pretty” output. Provides the response in a “pretty” format. In the case of JSON this means doing proper line breaking and indentation to make it readable. This will take extra time and increase the response size so it is advisable only to use this during debugging.
instance HasOptionalParam DeleteAttachment OptPretty where
  applyOptionalParam req (OptPretty xs) =
    req `setQuery` toQuery ("opt_pretty", Just xs)

-- | /Optional Param/ "opt_fields" - Defines fields to return. Some requests return *compact* representations of objects in order to conserve resources and complete the request more efficiently. Other times requests return more information than you may need. This option allows you to list the exact set of fields that the API should be sure to return for the objects. The field names should be provided as paths, described below. The id of included objects will always be returned, regardless of the field options.
instance HasOptionalParam DeleteAttachment OptFields where
  applyOptionalParam req (OptFields xs) =
    req `setQuery` toQueryColl CommaSeparated ("opt_fields", Just xs)
-- | @application/json@
instance Produces DeleteAttachment MimeJSON


-- *** getAttachment

-- | @GET \/attachments\/{attachment_gid}@
-- 
-- Get an attachment
-- 
-- Get the full record for a single attachment.
-- 
-- AuthMethod: 'AuthOAuthOauth2', 'AuthBasicPersonalAccessToken'
-- 
getAttachment 
  :: AttachmentGid -- ^ "attachmentGid" -  Globally unique identifier for the attachment.
  -> AsanaRequest GetAttachment MimeNoContent InlineResponse200 MimeJSON
getAttachment (AttachmentGid attachmentGid) =
  _mkRequest "GET" ["/attachments/",toPath attachmentGid]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthOauth2)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicPersonalAccessToken)

data GetAttachment  

-- | /Optional Param/ "opt_pretty" - Provides “pretty” output. Provides the response in a “pretty” format. In the case of JSON this means doing proper line breaking and indentation to make it readable. This will take extra time and increase the response size so it is advisable only to use this during debugging.
instance HasOptionalParam GetAttachment OptPretty where
  applyOptionalParam req (OptPretty xs) =
    req `setQuery` toQuery ("opt_pretty", Just xs)

-- | /Optional Param/ "opt_fields" - Defines fields to return. Some requests return *compact* representations of objects in order to conserve resources and complete the request more efficiently. Other times requests return more information than you may need. This option allows you to list the exact set of fields that the API should be sure to return for the objects. The field names should be provided as paths, described below. The id of included objects will always be returned, regardless of the field options.
instance HasOptionalParam GetAttachment OptFields where
  applyOptionalParam req (OptFields xs) =
    req `setQuery` toQueryColl CommaSeparated ("opt_fields", Just xs)
-- | @application/json@
instance Produces GetAttachment MimeJSON


-- *** getAttachmentsForTask

-- | @GET \/tasks\/{task_gid}\/attachments@
-- 
-- Get attachments for a task
-- 
-- Returns the compact records for all attachments on the task.
-- 
-- AuthMethod: 'AuthOAuthOauth2', 'AuthBasicPersonalAccessToken'
-- 
getAttachmentsForTask 
  :: TaskGid -- ^ "taskGid" -  The task to operate on.
  -> AsanaRequest GetAttachmentsForTask MimeNoContent InlineResponse2002 MimeJSON
getAttachmentsForTask (TaskGid taskGid) =
  _mkRequest "GET" ["/tasks/",toPath taskGid,"/attachments"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthOauth2)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicPersonalAccessToken)

data GetAttachmentsForTask  

-- | /Optional Param/ "opt_pretty" - Provides “pretty” output. Provides the response in a “pretty” format. In the case of JSON this means doing proper line breaking and indentation to make it readable. This will take extra time and increase the response size so it is advisable only to use this during debugging.
instance HasOptionalParam GetAttachmentsForTask OptPretty where
  applyOptionalParam req (OptPretty xs) =
    req `setQuery` toQuery ("opt_pretty", Just xs)

-- | /Optional Param/ "opt_fields" - Defines fields to return. Some requests return *compact* representations of objects in order to conserve resources and complete the request more efficiently. Other times requests return more information than you may need. This option allows you to list the exact set of fields that the API should be sure to return for the objects. The field names should be provided as paths, described below. The id of included objects will always be returned, regardless of the field options.
instance HasOptionalParam GetAttachmentsForTask OptFields where
  applyOptionalParam req (OptFields xs) =
    req `setQuery` toQueryColl CommaSeparated ("opt_fields", Just xs)

-- | /Optional Param/ "limit" - Results per page. The number of objects to return per page. The value must be between 1 and 100.
instance HasOptionalParam GetAttachmentsForTask Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "offset" - Offset token. An offset to the next page returned by the API. A pagination request will return an offset token, which can be used as an input parameter to the next request. If an offset is not passed in, the API will return the first page of results. 'Note: You can only pass in an offset that was returned to you via a previously paginated request.'
instance HasOptionalParam GetAttachmentsForTask Offset where
  applyOptionalParam req (Offset xs) =
    req `setQuery` toQuery ("offset", Just xs)
-- | @application/json@
instance Produces GetAttachmentsForTask MimeJSON

