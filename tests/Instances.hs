{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import Asana.Model
import Asana.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays
    
-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)
    
arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models
 
instance Arbitrary AddCustomFieldSettingRequest where
  arbitrary = sized genAddCustomFieldSettingRequest

genAddCustomFieldSettingRequest :: Int -> Gen AddCustomFieldSettingRequest
genAddCustomFieldSettingRequest n =
  AddCustomFieldSettingRequest
    <$> arbitrary -- addCustomFieldSettingRequestCustomField :: Text
    <*> arbitraryReducedMaybe n -- addCustomFieldSettingRequestIsImportant :: Maybe Bool
    <*> arbitraryReducedMaybe n -- addCustomFieldSettingRequestInsertBefore :: Maybe Text
    <*> arbitraryReducedMaybe n -- addCustomFieldSettingRequestInsertAfter :: Maybe Text
  
instance Arbitrary AddFollowersRequest where
  arbitrary = sized genAddFollowersRequest

genAddFollowersRequest :: Int -> Gen AddFollowersRequest
genAddFollowersRequest n =
  AddFollowersRequest
    <$> arbitrary -- addFollowersRequestFollowers :: Text
  
instance Arbitrary AddMembersRequest where
  arbitrary = sized genAddMembersRequest

genAddMembersRequest :: Int -> Gen AddMembersRequest
genAddMembersRequest n =
  AddMembersRequest
    <$> arbitrary -- addMembersRequestMembers :: Text
  
instance Arbitrary AsanaNamedResource where
  arbitrary = sized genAsanaNamedResource

genAsanaNamedResource :: Int -> Gen AsanaNamedResource
genAsanaNamedResource n =
  AsanaNamedResource
    <$> arbitraryReducedMaybe n -- asanaNamedResourceGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- asanaNamedResourceResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- asanaNamedResourceName :: Maybe Text
  
instance Arbitrary AsanaNamedResourceAllOf where
  arbitrary = sized genAsanaNamedResourceAllOf

genAsanaNamedResourceAllOf :: Int -> Gen AsanaNamedResourceAllOf
genAsanaNamedResourceAllOf n =
  AsanaNamedResourceAllOf
    <$> arbitraryReducedMaybe n -- asanaNamedResourceAllOfName :: Maybe Text
  
instance Arbitrary AsanaResource where
  arbitrary = sized genAsanaResource

genAsanaResource :: Int -> Gen AsanaResource
genAsanaResource n =
  AsanaResource
    <$> arbitraryReducedMaybe n -- asanaResourceGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- asanaResourceResourceType :: Maybe Text
  
instance Arbitrary AttachmentBase where
  arbitrary = sized genAttachmentBase

genAttachmentBase :: Int -> Gen AttachmentBase
genAttachmentBase n =
  
  pure AttachmentBase
   
instance Arbitrary AttachmentCompact where
  arbitrary = sized genAttachmentCompact

genAttachmentCompact :: Int -> Gen AttachmentCompact
genAttachmentCompact n =
  AttachmentCompact
    <$> arbitraryReducedMaybe n -- attachmentCompactGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- attachmentCompactResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- attachmentCompactName :: Maybe Text
  
instance Arbitrary AttachmentCompactAllOf where
  arbitrary = sized genAttachmentCompactAllOf

genAttachmentCompactAllOf :: Int -> Gen AttachmentCompactAllOf
genAttachmentCompactAllOf n =
  AttachmentCompactAllOf
    <$> arbitraryReducedMaybe n -- attachmentCompactAllOfName :: Maybe Text
  
instance Arbitrary AttachmentRequest where
  arbitrary = sized genAttachmentRequest

genAttachmentRequest :: Int -> Gen AttachmentRequest
genAttachmentRequest n =
  AttachmentRequest
    <$> arbitraryReducedMaybe n -- attachmentRequestFile :: Maybe FilePath
  
instance Arbitrary AttachmentResponse where
  arbitrary = sized genAttachmentResponse

genAttachmentResponse :: Int -> Gen AttachmentResponse
genAttachmentResponse n =
  AttachmentResponse
    <$> arbitraryReducedMaybe n -- attachmentResponseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- attachmentResponseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- attachmentResponseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- attachmentResponseCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- attachmentResponseDownloadUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- attachmentResponseHost :: Maybe Text
    <*> arbitraryReducedMaybe n -- attachmentResponseParent :: Maybe TaskCompact
    <*> arbitraryReducedMaybe n -- attachmentResponseViewUrl :: Maybe Text
  
instance Arbitrary AttachmentResponseAllOf where
  arbitrary = sized genAttachmentResponseAllOf

genAttachmentResponseAllOf :: Int -> Gen AttachmentResponseAllOf
genAttachmentResponseAllOf n =
  AttachmentResponseAllOf
    <$> arbitraryReducedMaybe n -- attachmentResponseAllOfCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- attachmentResponseAllOfDownloadUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- attachmentResponseAllOfHost :: Maybe Text
    <*> arbitraryReducedMaybe n -- attachmentResponseAllOfParent :: Maybe TaskCompact
    <*> arbitraryReducedMaybe n -- attachmentResponseAllOfViewUrl :: Maybe Text
  
instance Arbitrary BatchRequest where
  arbitrary = sized genBatchRequest

genBatchRequest :: Int -> Gen BatchRequest
genBatchRequest n =
  BatchRequest
    <$> arbitraryReducedMaybe n -- batchRequestActions :: Maybe [BatchRequestAction]
  
instance Arbitrary BatchRequestAction where
  arbitrary = sized genBatchRequestAction

genBatchRequestAction :: Int -> Gen BatchRequestAction
genBatchRequestAction n =
  BatchRequestAction
    <$> arbitrary -- batchRequestActionRelativePath :: Text
    <*> arbitrary -- batchRequestActionMethod :: E'Method
    <*> arbitraryReducedMaybeValue n -- batchRequestActionData :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- batchRequestActionOptions :: Maybe BatchRequestActionOptions
  
instance Arbitrary BatchRequestActionOptions where
  arbitrary = sized genBatchRequestActionOptions

genBatchRequestActionOptions :: Int -> Gen BatchRequestActionOptions
genBatchRequestActionOptions n =
  BatchRequestActionOptions
    <$> arbitraryReducedMaybe n -- batchRequestActionOptionsLimit :: Maybe Int
    <*> arbitraryReducedMaybe n -- batchRequestActionOptionsOffset :: Maybe Int
    <*> arbitraryReducedMaybe n -- batchRequestActionOptionsFields :: Maybe [Text]
  
instance Arbitrary BatchResponse where
  arbitrary = sized genBatchResponse

genBatchResponse :: Int -> Gen BatchResponse
genBatchResponse n =
  BatchResponse
    <$> arbitraryReducedMaybe n -- batchResponseStatusCode :: Maybe Int
    <*> arbitraryReducedMaybeValue n -- batchResponseHeaders :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- batchResponseBody :: Maybe A.Value
  
instance Arbitrary CustomFieldBase where
  arbitrary = sized genCustomFieldBase

genCustomFieldBase :: Int -> Gen CustomFieldBase
genCustomFieldBase n =
  CustomFieldBase
    <$> arbitraryReducedMaybe n -- customFieldBaseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldBaseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldBaseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldBaseResourceSubtype :: Maybe E'ResourceSubtype
    <*> arbitraryReducedMaybe n -- customFieldBaseType :: Maybe E'ResourceSubtype
    <*> arbitraryReducedMaybe n -- customFieldBaseEnumOptions :: Maybe [EnumOption]
    <*> arbitraryReducedMaybe n -- customFieldBaseEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- customFieldBaseNumberValue :: Maybe Double
    <*> arbitraryReducedMaybe n -- customFieldBaseTextValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldBaseDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldBasePrecision :: Maybe Int
    <*> arbitraryReducedMaybe n -- customFieldBaseFormat :: Maybe E'Format
    <*> arbitraryReducedMaybe n -- customFieldBaseCurrencyCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldBaseCustomLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldBaseCustomLabelPosition :: Maybe E'CustomLabelPosition
    <*> arbitraryReducedMaybe n -- customFieldBaseIsGlobalToWorkspace :: Maybe Bool
    <*> arbitraryReducedMaybe n -- customFieldBaseHasNotificationsEnabled :: Maybe Bool
  
instance Arbitrary CustomFieldBaseAllOf where
  arbitrary = sized genCustomFieldBaseAllOf

genCustomFieldBaseAllOf :: Int -> Gen CustomFieldBaseAllOf
genCustomFieldBaseAllOf n =
  CustomFieldBaseAllOf
    <$> arbitraryReducedMaybe n -- customFieldBaseAllOfDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldBaseAllOfEnumOptions :: Maybe [EnumOption]
    <*> arbitraryReducedMaybe n -- customFieldBaseAllOfPrecision :: Maybe Int
    <*> arbitraryReducedMaybe n -- customFieldBaseAllOfFormat :: Maybe E'Format
    <*> arbitraryReducedMaybe n -- customFieldBaseAllOfCurrencyCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldBaseAllOfCustomLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldBaseAllOfCustomLabelPosition :: Maybe E'CustomLabelPosition
    <*> arbitraryReducedMaybe n -- customFieldBaseAllOfIsGlobalToWorkspace :: Maybe Bool
    <*> arbitraryReducedMaybe n -- customFieldBaseAllOfHasNotificationsEnabled :: Maybe Bool
  
instance Arbitrary CustomFieldCompact where
  arbitrary = sized genCustomFieldCompact

genCustomFieldCompact :: Int -> Gen CustomFieldCompact
genCustomFieldCompact n =
  CustomFieldCompact
    <$> arbitraryReducedMaybe n -- customFieldCompactGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldCompactResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldCompactName :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldCompactResourceSubtype :: Maybe E'ResourceSubtype
    <*> arbitraryReducedMaybe n -- customFieldCompactType :: Maybe E'ResourceSubtype
    <*> arbitraryReducedMaybe n -- customFieldCompactEnumOptions :: Maybe [EnumOption]
    <*> arbitraryReducedMaybe n -- customFieldCompactEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- customFieldCompactNumberValue :: Maybe Double
    <*> arbitraryReducedMaybe n -- customFieldCompactTextValue :: Maybe Text
  
instance Arbitrary CustomFieldCompactAllOf where
  arbitrary = sized genCustomFieldCompactAllOf

genCustomFieldCompactAllOf :: Int -> Gen CustomFieldCompactAllOf
genCustomFieldCompactAllOf n =
  CustomFieldCompactAllOf
    <$> arbitraryReducedMaybe n -- customFieldCompactAllOfName :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldCompactAllOfResourceSubtype :: Maybe E'ResourceSubtype
    <*> arbitraryReducedMaybe n -- customFieldCompactAllOfType :: Maybe E'ResourceSubtype
    <*> arbitraryReducedMaybe n -- customFieldCompactAllOfEnumOptions :: Maybe [EnumOption]
    <*> arbitraryReducedMaybe n -- customFieldCompactAllOfEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- customFieldCompactAllOfNumberValue :: Maybe Double
    <*> arbitraryReducedMaybe n -- customFieldCompactAllOfTextValue :: Maybe Text
  
instance Arbitrary CustomFieldRequest where
  arbitrary = sized genCustomFieldRequest

genCustomFieldRequest :: Int -> Gen CustomFieldRequest
genCustomFieldRequest n =
  CustomFieldRequest
    <$> arbitraryReducedMaybe n -- customFieldRequestGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldRequestResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldRequestName :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldRequestResourceSubtype :: Maybe E'ResourceSubtype
    <*> arbitraryReducedMaybe n -- customFieldRequestType :: Maybe E'ResourceSubtype
    <*> arbitraryReducedMaybe n -- customFieldRequestEnumOptions :: Maybe [EnumOption]
    <*> arbitraryReducedMaybe n -- customFieldRequestEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- customFieldRequestNumberValue :: Maybe Double
    <*> arbitraryReducedMaybe n -- customFieldRequestTextValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldRequestDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldRequestPrecision :: Maybe Int
    <*> arbitraryReducedMaybe n -- customFieldRequestFormat :: Maybe E'Format
    <*> arbitraryReducedMaybe n -- customFieldRequestCurrencyCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldRequestCustomLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldRequestCustomLabelPosition :: Maybe E'CustomLabelPosition
    <*> arbitraryReducedMaybe n -- customFieldRequestIsGlobalToWorkspace :: Maybe Bool
    <*> arbitraryReducedMaybe n -- customFieldRequestHasNotificationsEnabled :: Maybe Bool
    <*> arbitrary -- customFieldRequestWorkspace :: Text
  
instance Arbitrary CustomFieldRequestAllOf where
  arbitrary = sized genCustomFieldRequestAllOf

genCustomFieldRequestAllOf :: Int -> Gen CustomFieldRequestAllOf
genCustomFieldRequestAllOf n =
  CustomFieldRequestAllOf
    <$> arbitrary -- customFieldRequestAllOfWorkspace :: Text
  
instance Arbitrary CustomFieldResponse where
  arbitrary = sized genCustomFieldResponse

genCustomFieldResponse :: Int -> Gen CustomFieldResponse
genCustomFieldResponse n =
  CustomFieldResponse
    <$> arbitraryReducedMaybe n -- customFieldResponseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldResponseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldResponseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldResponseResourceSubtype :: Maybe E'ResourceSubtype
    <*> arbitraryReducedMaybe n -- customFieldResponseType :: Maybe E'ResourceSubtype
    <*> arbitraryReducedMaybe n -- customFieldResponseEnumOptions :: Maybe [EnumOption]
    <*> arbitraryReducedMaybe n -- customFieldResponseEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- customFieldResponseNumberValue :: Maybe Double
    <*> arbitraryReducedMaybe n -- customFieldResponseTextValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldResponseDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldResponsePrecision :: Maybe Int
    <*> arbitraryReducedMaybe n -- customFieldResponseFormat :: Maybe E'Format
    <*> arbitraryReducedMaybe n -- customFieldResponseCurrencyCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldResponseCustomLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldResponseCustomLabelPosition :: Maybe E'CustomLabelPosition
    <*> arbitraryReducedMaybe n -- customFieldResponseIsGlobalToWorkspace :: Maybe Bool
    <*> arbitraryReducedMaybe n -- customFieldResponseHasNotificationsEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- customFieldResponseEnumValue :: Maybe EnumOption
  
instance Arbitrary CustomFieldResponseAllOf where
  arbitrary = sized genCustomFieldResponseAllOf

genCustomFieldResponseAllOf :: Int -> Gen CustomFieldResponseAllOf
genCustomFieldResponseAllOf n =
  CustomFieldResponseAllOf
    <$> arbitraryReducedMaybe n -- customFieldResponseAllOfEnumValue :: Maybe EnumOption
  
instance Arbitrary CustomFieldSettingBase where
  arbitrary = sized genCustomFieldSettingBase

genCustomFieldSettingBase :: Int -> Gen CustomFieldSettingBase
genCustomFieldSettingBase n =
  
  pure CustomFieldSettingBase
   
instance Arbitrary CustomFieldSettingCompact where
  arbitrary = sized genCustomFieldSettingCompact

genCustomFieldSettingCompact :: Int -> Gen CustomFieldSettingCompact
genCustomFieldSettingCompact n =
  CustomFieldSettingCompact
    <$> arbitraryReducedMaybe n -- customFieldSettingCompactGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldSettingCompactResourceType :: Maybe Text
  
instance Arbitrary CustomFieldSettingResponse where
  arbitrary = sized genCustomFieldSettingResponse

genCustomFieldSettingResponse :: Int -> Gen CustomFieldSettingResponse
genCustomFieldSettingResponse n =
  CustomFieldSettingResponse
    <$> arbitraryReducedMaybe n -- customFieldSettingResponseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldSettingResponseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- customFieldSettingResponseProject :: Maybe ProjectCompact
    <*> arbitraryReducedMaybe n -- customFieldSettingResponseIsImportant :: Maybe Bool
    <*> arbitraryReducedMaybe n -- customFieldSettingResponseParent :: Maybe ProjectCompact
    <*> arbitraryReducedMaybe n -- customFieldSettingResponseCustomField :: Maybe CustomFieldResponse
  
instance Arbitrary CustomFieldSettingResponseAllOf where
  arbitrary = sized genCustomFieldSettingResponseAllOf

genCustomFieldSettingResponseAllOf :: Int -> Gen CustomFieldSettingResponseAllOf
genCustomFieldSettingResponseAllOf n =
  CustomFieldSettingResponseAllOf
    <$> arbitraryReducedMaybe n -- customFieldSettingResponseAllOfProject :: Maybe ProjectCompact
    <*> arbitraryReducedMaybe n -- customFieldSettingResponseAllOfIsImportant :: Maybe Bool
    <*> arbitraryReducedMaybe n -- customFieldSettingResponseAllOfParent :: Maybe ProjectCompact
    <*> arbitraryReducedMaybe n -- customFieldSettingResponseAllOfCustomField :: Maybe CustomFieldResponse
  
instance Arbitrary EnumOption where
  arbitrary = sized genEnumOption

genEnumOption :: Int -> Gen EnumOption
genEnumOption n =
  EnumOption
    <$> arbitraryReducedMaybe n -- enumOptionGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- enumOptionResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- enumOptionName :: Maybe Text
    <*> arbitraryReducedMaybe n -- enumOptionEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- enumOptionColor :: Maybe Text
  
instance Arbitrary EnumOptionAllOf where
  arbitrary = sized genEnumOptionAllOf

genEnumOptionAllOf :: Int -> Gen EnumOptionAllOf
genEnumOptionAllOf n =
  EnumOptionAllOf
    <$> arbitraryReducedMaybe n -- enumOptionAllOfName :: Maybe Text
    <*> arbitraryReducedMaybe n -- enumOptionAllOfEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- enumOptionAllOfColor :: Maybe Text
  
instance Arbitrary EnumOptionBase where
  arbitrary = sized genEnumOptionBase

genEnumOptionBase :: Int -> Gen EnumOptionBase
genEnumOptionBase n =
  
  pure EnumOptionBase
   
instance Arbitrary EnumOptionInsertRequest where
  arbitrary = sized genEnumOptionInsertRequest

genEnumOptionInsertRequest :: Int -> Gen EnumOptionInsertRequest
genEnumOptionInsertRequest n =
  EnumOptionInsertRequest
    <$> arbitrary -- enumOptionInsertRequestEnumOption :: Text
    <*> arbitraryReducedMaybe n -- enumOptionInsertRequestBeforeEnumOption :: Maybe Text
    <*> arbitraryReducedMaybe n -- enumOptionInsertRequestAfterEnumOption :: Maybe Text
  
instance Arbitrary EnumOptionRequest where
  arbitrary = sized genEnumOptionRequest

genEnumOptionRequest :: Int -> Gen EnumOptionRequest
genEnumOptionRequest n =
  EnumOptionRequest
    <$> arbitraryReducedMaybe n -- enumOptionRequestGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- enumOptionRequestResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- enumOptionRequestName :: Maybe Text
    <*> arbitraryReducedMaybe n -- enumOptionRequestEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- enumOptionRequestColor :: Maybe Text
    <*> arbitraryReducedMaybe n -- enumOptionRequestInsertBefore :: Maybe Text
    <*> arbitraryReducedMaybe n -- enumOptionRequestInsertAfter :: Maybe Text
  
instance Arbitrary EnumOptionRequestAllOf where
  arbitrary = sized genEnumOptionRequestAllOf

genEnumOptionRequestAllOf :: Int -> Gen EnumOptionRequestAllOf
genEnumOptionRequestAllOf n =
  EnumOptionRequestAllOf
    <$> arbitraryReducedMaybe n -- enumOptionRequestAllOfInsertBefore :: Maybe Text
    <*> arbitraryReducedMaybe n -- enumOptionRequestAllOfInsertAfter :: Maybe Text
  
instance Arbitrary Error where
  arbitrary = sized genError

genError :: Int -> Gen Error
genError n =
  Error
    <$> arbitraryReducedMaybe n -- errorMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- errorHelp :: Maybe Text
    <*> arbitraryReducedMaybe n -- errorPhrase :: Maybe Text
  
instance Arbitrary ErrorResponse where
  arbitrary = sized genErrorResponse

genErrorResponse :: Int -> Gen ErrorResponse
genErrorResponse n =
  ErrorResponse
    <$> arbitraryReducedMaybe n -- errorResponseErrors :: Maybe [Error]
  
instance Arbitrary EventResponse where
  arbitrary = sized genEventResponse

genEventResponse :: Int -> Gen EventResponse
genEventResponse n =
  EventResponse
    <$> arbitraryReducedMaybe n -- eventResponseUser :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- eventResponseResource :: Maybe AsanaNamedResource
    <*> arbitraryReducedMaybe n -- eventResponseType :: Maybe Text
    <*> arbitraryReducedMaybe n -- eventResponseAction :: Maybe Text
    <*> arbitraryReducedMaybe n -- eventResponseParent :: Maybe AsanaNamedResource
    <*> arbitraryReducedMaybe n -- eventResponseCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- eventResponseChange :: Maybe EventResponseChange
  
instance Arbitrary EventResponseChange where
  arbitrary = sized genEventResponseChange

genEventResponseChange :: Int -> Gen EventResponseChange
genEventResponseChange n =
  EventResponseChange
    <$> arbitraryReducedMaybe n -- eventResponseChangeField :: Maybe Text
    <*> arbitraryReducedMaybe n -- eventResponseChangeAction :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- eventResponseChangeNewValue :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- eventResponseChangeAddedValue :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- eventResponseChangeRemovedValue :: Maybe A.Value
  
instance Arbitrary InlineObject where
  arbitrary = sized genInlineObject

genInlineObject :: Int -> Gen InlineObject
genInlineObject n =
  InlineObject
    <$> arbitraryReducedMaybe n -- inlineObjectData :: Maybe BatchRequest
  
instance Arbitrary InlineObject1 where
  arbitrary = sized genInlineObject1

genInlineObject1 :: Int -> Gen InlineObject1
genInlineObject1 n =
  InlineObject1
    <$> arbitraryReducedMaybe n -- inlineObject1Data :: Maybe CustomFieldRequest
  
instance Arbitrary InlineObject10 where
  arbitrary = sized genInlineObject10

genInlineObject10 :: Int -> Gen InlineObject10
genInlineObject10 n =
  InlineObject10
    <$> arbitraryReducedMaybe n -- inlineObject10Data :: Maybe PortfolioRemoveItemRequest
  
instance Arbitrary InlineObject11 where
  arbitrary = sized genInlineObject11

genInlineObject11 :: Int -> Gen InlineObject11
genInlineObject11 n =
  InlineObject11
    <$> arbitraryReducedMaybe n -- inlineObject11Data :: Maybe AddCustomFieldSettingRequest
  
instance Arbitrary InlineObject12 where
  arbitrary = sized genInlineObject12

genInlineObject12 :: Int -> Gen InlineObject12
genInlineObject12 n =
  InlineObject12
    <$> arbitraryReducedMaybe n -- inlineObject12Data :: Maybe RemoveCustomFieldSettingRequest
  
instance Arbitrary InlineObject13 where
  arbitrary = sized genInlineObject13

genInlineObject13 :: Int -> Gen InlineObject13
genInlineObject13 n =
  InlineObject13
    <$> arbitraryReducedMaybe n -- inlineObject13Data :: Maybe AddMembersRequest
  
instance Arbitrary InlineObject14 where
  arbitrary = sized genInlineObject14

genInlineObject14 :: Int -> Gen InlineObject14
genInlineObject14 n =
  InlineObject14
    <$> arbitraryReducedMaybe n -- inlineObject14Data :: Maybe RemoveMembersRequest
  
instance Arbitrary InlineObject15 where
  arbitrary = sized genInlineObject15

genInlineObject15 :: Int -> Gen InlineObject15
genInlineObject15 n =
  InlineObject15
    <$> arbitraryReducedMaybe n -- inlineObject15Data :: Maybe ProjectStatusBase
  
instance Arbitrary InlineObject16 where
  arbitrary = sized genInlineObject16

genInlineObject16 :: Int -> Gen InlineObject16
genInlineObject16 n =
  InlineObject16
    <$> arbitraryReducedMaybe n -- inlineObject16Data :: Maybe ProjectRequest
  
instance Arbitrary InlineObject17 where
  arbitrary = sized genInlineObject17

genInlineObject17 :: Int -> Gen InlineObject17
genInlineObject17 n =
  InlineObject17
    <$> arbitraryReducedMaybe n -- inlineObject17Data :: Maybe ProjectRequest
  
instance Arbitrary InlineObject18 where
  arbitrary = sized genInlineObject18

genInlineObject18 :: Int -> Gen InlineObject18
genInlineObject18 n =
  InlineObject18
    <$> arbitraryReducedMaybe n -- inlineObject18Data :: Maybe ProjectDuplicateRequest
  
instance Arbitrary InlineObject19 where
  arbitrary = sized genInlineObject19

genInlineObject19 :: Int -> Gen InlineObject19
genInlineObject19 n =
  InlineObject19
    <$> arbitraryReducedMaybe n -- inlineObject19Data :: Maybe ProjectRequest
  
instance Arbitrary InlineObject2 where
  arbitrary = sized genInlineObject2

genInlineObject2 :: Int -> Gen InlineObject2
genInlineObject2 n =
  InlineObject2
    <$> arbitraryReducedMaybe n -- inlineObject2Data :: Maybe CustomFieldRequest
  
instance Arbitrary InlineObject20 where
  arbitrary = sized genInlineObject20

genInlineObject20 :: Int -> Gen InlineObject20
genInlineObject20 n =
  InlineObject20
    <$> arbitraryReducedMaybe n -- inlineObject20Data :: Maybe ProjectRequest
  
instance Arbitrary InlineObject21 where
  arbitrary = sized genInlineObject21

genInlineObject21 :: Int -> Gen InlineObject21
genInlineObject21 n =
  InlineObject21
    <$> arbitraryReducedMaybe n -- inlineObject21Data :: Maybe AddCustomFieldSettingRequest
  
instance Arbitrary InlineObject22 where
  arbitrary = sized genInlineObject22

genInlineObject22 :: Int -> Gen InlineObject22
genInlineObject22 n =
  InlineObject22
    <$> arbitraryReducedMaybe n -- inlineObject22Data :: Maybe RemoveCustomFieldSettingRequest
  
instance Arbitrary InlineObject23 where
  arbitrary = sized genInlineObject23

genInlineObject23 :: Int -> Gen InlineObject23
genInlineObject23 n =
  InlineObject23
    <$> arbitraryReducedMaybe n -- inlineObject23Data :: Maybe AddMembersRequest
  
instance Arbitrary InlineObject24 where
  arbitrary = sized genInlineObject24

genInlineObject24 :: Int -> Gen InlineObject24
genInlineObject24 n =
  InlineObject24
    <$> arbitraryReducedMaybe n -- inlineObject24Data :: Maybe RemoveMembersRequest
  
instance Arbitrary InlineObject25 where
  arbitrary = sized genInlineObject25

genInlineObject25 :: Int -> Gen InlineObject25
genInlineObject25 n =
  InlineObject25
    <$> arbitraryReducedMaybe n -- inlineObject25Data :: Maybe AddFollowersRequest
  
instance Arbitrary InlineObject26 where
  arbitrary = sized genInlineObject26

genInlineObject26 :: Int -> Gen InlineObject26
genInlineObject26 n =
  InlineObject26
    <$> arbitraryReducedMaybe n -- inlineObject26Data :: Maybe RemoveFollowersRequest
  
instance Arbitrary InlineObject27 where
  arbitrary = sized genInlineObject27

genInlineObject27 :: Int -> Gen InlineObject27
genInlineObject27 n =
  InlineObject27
    <$> arbitraryReducedMaybe n -- inlineObject27Data :: Maybe SectionRequest
  
instance Arbitrary InlineObject28 where
  arbitrary = sized genInlineObject28

genInlineObject28 :: Int -> Gen InlineObject28
genInlineObject28 n =
  InlineObject28
    <$> arbitraryReducedMaybe n -- inlineObject28Data :: Maybe SectionRequest
  
instance Arbitrary InlineObject29 where
  arbitrary = sized genInlineObject29

genInlineObject29 :: Int -> Gen InlineObject29
genInlineObject29 n =
  InlineObject29
    <$> arbitraryReducedMaybe n -- inlineObject29Data :: Maybe SectionTaskInsertRequest
  
instance Arbitrary InlineObject3 where
  arbitrary = sized genInlineObject3

genInlineObject3 :: Int -> Gen InlineObject3
genInlineObject3 n =
  InlineObject3
    <$> arbitraryReducedMaybe n -- inlineObject3Data :: Maybe EnumOptionRequest
  
instance Arbitrary InlineObject30 where
  arbitrary = sized genInlineObject30

genInlineObject30 :: Int -> Gen InlineObject30
genInlineObject30 n =
  InlineObject30
    <$> arbitraryReducedMaybe n -- inlineObject30Data :: Maybe ProjectSectionInsertRequest
  
instance Arbitrary InlineObject31 where
  arbitrary = sized genInlineObject31

genInlineObject31 :: Int -> Gen InlineObject31
genInlineObject31 n =
  InlineObject31
    <$> arbitraryReducedMaybe n -- inlineObject31Data :: Maybe StoryBase
  
instance Arbitrary InlineObject32 where
  arbitrary = sized genInlineObject32

genInlineObject32 :: Int -> Gen InlineObject32
genInlineObject32 n =
  InlineObject32
    <$> arbitraryReducedMaybe n -- inlineObject32Data :: Maybe StoryBase
  
instance Arbitrary InlineObject33 where
  arbitrary = sized genInlineObject33

genInlineObject33 :: Int -> Gen InlineObject33
genInlineObject33 n =
  InlineObject33
    <$> arbitraryReducedMaybe n -- inlineObject33Data :: Maybe TagRequest
  
instance Arbitrary InlineObject34 where
  arbitrary = sized genInlineObject34

genInlineObject34 :: Int -> Gen InlineObject34
genInlineObject34 n =
  InlineObject34
    <$> arbitraryReducedMaybe n -- inlineObject34Data :: Maybe TagResponse
  
instance Arbitrary InlineObject35 where
  arbitrary = sized genInlineObject35

genInlineObject35 :: Int -> Gen InlineObject35
genInlineObject35 n =
  InlineObject35
    <$> arbitraryReducedMaybe n -- inlineObject35Data :: Maybe TaskRequest
  
instance Arbitrary InlineObject36 where
  arbitrary = sized genInlineObject36

genInlineObject36 :: Int -> Gen InlineObject36
genInlineObject36 n =
  InlineObject36
    <$> arbitraryReducedMaybe n -- inlineObject36Data :: Maybe TaskRequest
  
instance Arbitrary InlineObject37 where
  arbitrary = sized genInlineObject37

genInlineObject37 :: Int -> Gen InlineObject37
genInlineObject37 n =
  InlineObject37
    <$> arbitraryReducedMaybe n -- inlineObject37Data :: Maybe TaskDuplicateRequest
  
instance Arbitrary InlineObject38 where
  arbitrary = sized genInlineObject38

genInlineObject38 :: Int -> Gen InlineObject38
genInlineObject38 n =
  InlineObject38
    <$> arbitraryReducedMaybe n -- inlineObject38Data :: Maybe TaskRequest
  
instance Arbitrary InlineObject39 where
  arbitrary = sized genInlineObject39

genInlineObject39 :: Int -> Gen InlineObject39
genInlineObject39 n =
  InlineObject39
    <$> arbitraryReducedMaybe n -- inlineObject39Data :: Maybe TaskSetParentRequest
  
instance Arbitrary InlineObject4 where
  arbitrary = sized genInlineObject4

genInlineObject4 :: Int -> Gen InlineObject4
genInlineObject4 n =
  InlineObject4
    <$> arbitraryReducedMaybe n -- inlineObject4Data :: Maybe EnumOptionInsertRequest
  
instance Arbitrary InlineObject40 where
  arbitrary = sized genInlineObject40

genInlineObject40 :: Int -> Gen InlineObject40
genInlineObject40 n =
  InlineObject40
    <$> arbitraryReducedMaybe n -- inlineObject40Data :: Maybe ModifyDependenciesRequest
  
instance Arbitrary InlineObject41 where
  arbitrary = sized genInlineObject41

genInlineObject41 :: Int -> Gen InlineObject41
genInlineObject41 n =
  InlineObject41
    <$> arbitraryReducedMaybe n -- inlineObject41Data :: Maybe ModifyDependenciesRequest
  
instance Arbitrary InlineObject42 where
  arbitrary = sized genInlineObject42

genInlineObject42 :: Int -> Gen InlineObject42
genInlineObject42 n =
  InlineObject42
    <$> arbitraryReducedMaybe n -- inlineObject42Data :: Maybe ModifyDependentsRequest
  
instance Arbitrary InlineObject43 where
  arbitrary = sized genInlineObject43

genInlineObject43 :: Int -> Gen InlineObject43
genInlineObject43 n =
  InlineObject43
    <$> arbitraryReducedMaybe n -- inlineObject43Data :: Maybe ModifyDependentsRequest
  
instance Arbitrary InlineObject44 where
  arbitrary = sized genInlineObject44

genInlineObject44 :: Int -> Gen InlineObject44
genInlineObject44 n =
  InlineObject44
    <$> arbitraryReducedMaybe n -- inlineObject44Data :: Maybe TaskAddProjectRequest
  
instance Arbitrary InlineObject45 where
  arbitrary = sized genInlineObject45

genInlineObject45 :: Int -> Gen InlineObject45
genInlineObject45 n =
  InlineObject45
    <$> arbitraryReducedMaybe n -- inlineObject45Data :: Maybe TaskRemoveProjectRequest
  
instance Arbitrary InlineObject46 where
  arbitrary = sized genInlineObject46

genInlineObject46 :: Int -> Gen InlineObject46
genInlineObject46 n =
  InlineObject46
    <$> arbitraryReducedMaybe n -- inlineObject46Data :: Maybe TaskAddTagRequest
  
instance Arbitrary InlineObject47 where
  arbitrary = sized genInlineObject47

genInlineObject47 :: Int -> Gen InlineObject47
genInlineObject47 n =
  InlineObject47
    <$> arbitraryReducedMaybe n -- inlineObject47Data :: Maybe TaskRemoveTagRequest
  
instance Arbitrary InlineObject48 where
  arbitrary = sized genInlineObject48

genInlineObject48 :: Int -> Gen InlineObject48
genInlineObject48 n =
  InlineObject48
    <$> arbitraryReducedMaybe n -- inlineObject48Data :: Maybe TaskAddFollowersRequest
  
instance Arbitrary InlineObject49 where
  arbitrary = sized genInlineObject49

genInlineObject49 :: Int -> Gen InlineObject49
genInlineObject49 n =
  InlineObject49
    <$> arbitraryReducedMaybe n -- inlineObject49Data :: Maybe TaskRemoveFollowersRequest
  
instance Arbitrary InlineObject5 where
  arbitrary = sized genInlineObject5

genInlineObject5 :: Int -> Gen InlineObject5
genInlineObject5 n =
  InlineObject5
    <$> arbitraryReducedMaybe n -- inlineObject5Data :: Maybe EnumOptionRequest
  
instance Arbitrary InlineObject50 where
  arbitrary = sized genInlineObject50

genInlineObject50 :: Int -> Gen InlineObject50
genInlineObject50 n =
  InlineObject50
    <$> arbitraryReducedMaybe n -- inlineObject50Data :: Maybe TeamAddUserRequest
  
instance Arbitrary InlineObject51 where
  arbitrary = sized genInlineObject51

genInlineObject51 :: Int -> Gen InlineObject51
genInlineObject51 n =
  InlineObject51
    <$> arbitraryReducedMaybe n -- inlineObject51Data :: Maybe TeamRemoveUserRequest
  
instance Arbitrary InlineObject52 where
  arbitrary = sized genInlineObject52

genInlineObject52 :: Int -> Gen InlineObject52
genInlineObject52 n =
  InlineObject52
    <$> arbitraryReducedMaybe n -- inlineObject52Data :: Maybe WebhookRequest
  
instance Arbitrary InlineObject53 where
  arbitrary = sized genInlineObject53

genInlineObject53 :: Int -> Gen InlineObject53
genInlineObject53 n =
  InlineObject53
    <$> arbitraryReducedMaybe n -- inlineObject53Data :: Maybe WorkspaceCompact
  
instance Arbitrary InlineObject54 where
  arbitrary = sized genInlineObject54

genInlineObject54 :: Int -> Gen InlineObject54
genInlineObject54 n =
  InlineObject54
    <$> arbitraryReducedMaybe n -- inlineObject54Data :: Maybe WorkspaceAddUserRequest
  
instance Arbitrary InlineObject55 where
  arbitrary = sized genInlineObject55

genInlineObject55 :: Int -> Gen InlineObject55
genInlineObject55 n =
  InlineObject55
    <$> arbitraryReducedMaybe n -- inlineObject55Data :: Maybe WorkspaceRemoveUserRequest
  
instance Arbitrary InlineObject6 where
  arbitrary = sized genInlineObject6

genInlineObject6 :: Int -> Gen InlineObject6
genInlineObject6 n =
  InlineObject6
    <$> arbitraryReducedMaybe n -- inlineObject6Data :: Maybe OrganizationExportRequest
  
instance Arbitrary InlineObject7 where
  arbitrary = sized genInlineObject7

genInlineObject7 :: Int -> Gen InlineObject7
genInlineObject7 n =
  InlineObject7
    <$> arbitraryReducedMaybe n -- inlineObject7Data :: Maybe PortfolioRequest
  
instance Arbitrary InlineObject8 where
  arbitrary = sized genInlineObject8

genInlineObject8 :: Int -> Gen InlineObject8
genInlineObject8 n =
  InlineObject8
    <$> arbitraryReducedMaybe n -- inlineObject8Data :: Maybe PortfolioRequest
  
instance Arbitrary InlineObject9 where
  arbitrary = sized genInlineObject9

genInlineObject9 :: Int -> Gen InlineObject9
genInlineObject9 n =
  InlineObject9
    <$> arbitraryReducedMaybe n -- inlineObject9Data :: Maybe PortfolioAddItemRequest
  
instance Arbitrary InlineResponse200 where
  arbitrary = sized genInlineResponse200

genInlineResponse200 :: Int -> Gen InlineResponse200
genInlineResponse200 n =
  InlineResponse200
    <$> arbitraryReducedMaybe n -- inlineResponse200Data :: Maybe AttachmentResponse
  
instance Arbitrary InlineResponse2001 where
  arbitrary = sized genInlineResponse2001

genInlineResponse2001 :: Int -> Gen InlineResponse2001
genInlineResponse2001 n =
  InlineResponse2001
    <$> arbitraryReducedMaybeValue n -- inlineResponse2001Data :: Maybe A.Value
  
instance Arbitrary InlineResponse20010 where
  arbitrary = sized genInlineResponse20010

genInlineResponse20010 :: Int -> Gen InlineResponse20010
genInlineResponse20010 n =
  InlineResponse20010
    <$> arbitraryReducedMaybe n -- inlineResponse20010Data :: Maybe [PortfolioCompact]
  
instance Arbitrary InlineResponse20011 where
  arbitrary = sized genInlineResponse20011

genInlineResponse20011 :: Int -> Gen InlineResponse20011
genInlineResponse20011 n =
  InlineResponse20011
    <$> arbitraryReducedMaybe n -- inlineResponse20011Data :: Maybe [ProjectCompact]
  
instance Arbitrary InlineResponse20012 where
  arbitrary = sized genInlineResponse20012

genInlineResponse20012 :: Int -> Gen InlineResponse20012
genInlineResponse20012 n =
  InlineResponse20012
    <$> arbitraryReducedMaybe n -- inlineResponse20012Data :: Maybe ProjectMembershipResponse
  
instance Arbitrary InlineResponse20013 where
  arbitrary = sized genInlineResponse20013

genInlineResponse20013 :: Int -> Gen InlineResponse20013
genInlineResponse20013 n =
  InlineResponse20013
    <$> arbitraryReducedMaybe n -- inlineResponse20013Data :: Maybe [ProjectMembershipCompact]
  
instance Arbitrary InlineResponse20014 where
  arbitrary = sized genInlineResponse20014

genInlineResponse20014 :: Int -> Gen InlineResponse20014
genInlineResponse20014 n =
  InlineResponse20014
    <$> arbitraryReducedMaybe n -- inlineResponse20014Data :: Maybe ProjectStatusResponse
  
instance Arbitrary InlineResponse20015 where
  arbitrary = sized genInlineResponse20015

genInlineResponse20015 :: Int -> Gen InlineResponse20015
genInlineResponse20015 n =
  InlineResponse20015
    <$> arbitraryReducedMaybe n -- inlineResponse20015Data :: Maybe [ProjectStatusCompact]
  
instance Arbitrary InlineResponse20016 where
  arbitrary = sized genInlineResponse20016

genInlineResponse20016 :: Int -> Gen InlineResponse20016
genInlineResponse20016 n =
  InlineResponse20016
    <$> arbitraryReducedMaybe n -- inlineResponse20016Data :: Maybe TaskCountResponse
  
instance Arbitrary InlineResponse20017 where
  arbitrary = sized genInlineResponse20017

genInlineResponse20017 :: Int -> Gen InlineResponse20017
genInlineResponse20017 n =
  InlineResponse20017
    <$> arbitraryReducedMaybe n -- inlineResponse20017Data :: Maybe SectionResponse
  
instance Arbitrary InlineResponse20018 where
  arbitrary = sized genInlineResponse20018

genInlineResponse20018 :: Int -> Gen InlineResponse20018
genInlineResponse20018 n =
  InlineResponse20018
    <$> arbitraryReducedMaybe n -- inlineResponse20018Data :: Maybe [SectionCompact]
  
instance Arbitrary InlineResponse20019 where
  arbitrary = sized genInlineResponse20019

genInlineResponse20019 :: Int -> Gen InlineResponse20019
genInlineResponse20019 n =
  InlineResponse20019
    <$> arbitraryReducedMaybe n -- inlineResponse20019Data :: Maybe StoryResponse
  
instance Arbitrary InlineResponse2002 where
  arbitrary = sized genInlineResponse2002

genInlineResponse2002 :: Int -> Gen InlineResponse2002
genInlineResponse2002 n =
  InlineResponse2002
    <$> arbitraryReducedMaybe n -- inlineResponse2002Data :: Maybe [AttachmentCompact]
  
instance Arbitrary InlineResponse20020 where
  arbitrary = sized genInlineResponse20020

genInlineResponse20020 :: Int -> Gen InlineResponse20020
genInlineResponse20020 n =
  InlineResponse20020
    <$> arbitraryReducedMaybe n -- inlineResponse20020Data :: Maybe [StoryCompact]
  
instance Arbitrary InlineResponse20021 where
  arbitrary = sized genInlineResponse20021

genInlineResponse20021 :: Int -> Gen InlineResponse20021
genInlineResponse20021 n =
  InlineResponse20021
    <$> arbitraryReducedMaybe n -- inlineResponse20021Data :: Maybe [TagCompact]
  
instance Arbitrary InlineResponse20022 where
  arbitrary = sized genInlineResponse20022

genInlineResponse20022 :: Int -> Gen InlineResponse20022
genInlineResponse20022 n =
  InlineResponse20022
    <$> arbitraryReducedMaybe n -- inlineResponse20022Data :: Maybe [TaskCompact]
  
instance Arbitrary InlineResponse20023 where
  arbitrary = sized genInlineResponse20023

genInlineResponse20023 :: Int -> Gen InlineResponse20023
genInlineResponse20023 n =
  InlineResponse20023
    <$> arbitraryReducedMaybe n -- inlineResponse20023Data :: Maybe [A.Value]
  
instance Arbitrary InlineResponse20024 where
  arbitrary = sized genInlineResponse20024

genInlineResponse20024 :: Int -> Gen InlineResponse20024
genInlineResponse20024 n =
  InlineResponse20024
    <$> arbitraryReducedMaybe n -- inlineResponse20024Data :: Maybe TeamMembershipCompact
  
instance Arbitrary InlineResponse20025 where
  arbitrary = sized genInlineResponse20025

genInlineResponse20025 :: Int -> Gen InlineResponse20025
genInlineResponse20025 n =
  InlineResponse20025
    <$> arbitraryReducedMaybe n -- inlineResponse20025Data :: Maybe [TeamMembershipCompact]
  
instance Arbitrary InlineResponse20026 where
  arbitrary = sized genInlineResponse20026

genInlineResponse20026 :: Int -> Gen InlineResponse20026
genInlineResponse20026 n =
  InlineResponse20026
    <$> arbitraryReducedMaybe n -- inlineResponse20026Data :: Maybe TeamResponse
  
instance Arbitrary InlineResponse20027 where
  arbitrary = sized genInlineResponse20027

genInlineResponse20027 :: Int -> Gen InlineResponse20027
genInlineResponse20027 n =
  InlineResponse20027
    <$> arbitraryReducedMaybe n -- inlineResponse20027Data :: Maybe [TeamCompact]
  
instance Arbitrary InlineResponse20028 where
  arbitrary = sized genInlineResponse20028

genInlineResponse20028 :: Int -> Gen InlineResponse20028
genInlineResponse20028 n =
  InlineResponse20028
    <$> arbitraryReducedMaybe n -- inlineResponse20028Data :: Maybe UserResponse
  
instance Arbitrary InlineResponse20029 where
  arbitrary = sized genInlineResponse20029

genInlineResponse20029 :: Int -> Gen InlineResponse20029
genInlineResponse20029 n =
  InlineResponse20029
    <$> arbitraryReducedMaybe n -- inlineResponse20029Data :: Maybe [AsanaNamedResource]
  
instance Arbitrary InlineResponse2003 where
  arbitrary = sized genInlineResponse2003

genInlineResponse2003 :: Int -> Gen InlineResponse2003
genInlineResponse2003 n =
  InlineResponse2003
    <$> arbitraryReducedMaybe n -- inlineResponse2003Data :: Maybe [BatchResponse]
  
instance Arbitrary InlineResponse20030 where
  arbitrary = sized genInlineResponse20030

genInlineResponse20030 :: Int -> Gen InlineResponse20030
genInlineResponse20030 n =
  InlineResponse20030
    <$> arbitraryReducedMaybe n -- inlineResponse20030Data :: Maybe UserTaskListCompact
  
instance Arbitrary InlineResponse20031 where
  arbitrary = sized genInlineResponse20031

genInlineResponse20031 :: Int -> Gen InlineResponse20031
genInlineResponse20031 n =
  InlineResponse20031
    <$> arbitraryReducedMaybe n -- inlineResponse20031Data :: Maybe [UserCompact]
  
instance Arbitrary InlineResponse20032 where
  arbitrary = sized genInlineResponse20032

genInlineResponse20032 :: Int -> Gen InlineResponse20032
genInlineResponse20032 n =
  InlineResponse20032
    <$> arbitraryReducedMaybe n -- inlineResponse20032Data :: Maybe [AsanaNamedResource]
  
instance Arbitrary InlineResponse20033 where
  arbitrary = sized genInlineResponse20033

genInlineResponse20033 :: Int -> Gen InlineResponse20033
genInlineResponse20033 n =
  InlineResponse20033
    <$> arbitraryReducedMaybe n -- inlineResponse20033Data :: Maybe [WebhookResponse]
  
instance Arbitrary InlineResponse20034 where
  arbitrary = sized genInlineResponse20034

genInlineResponse20034 :: Int -> Gen InlineResponse20034
genInlineResponse20034 n =
  InlineResponse20034
    <$> arbitraryReducedMaybe n -- inlineResponse20034Data :: Maybe WorkspaceMembershipResponse
  
instance Arbitrary InlineResponse20035 where
  arbitrary = sized genInlineResponse20035

genInlineResponse20035 :: Int -> Gen InlineResponse20035
genInlineResponse20035 n =
  InlineResponse20035
    <$> arbitraryReducedMaybe n -- inlineResponse20035Data :: Maybe [WorkspaceMembershipCompact]
  
instance Arbitrary InlineResponse20036 where
  arbitrary = sized genInlineResponse20036

genInlineResponse20036 :: Int -> Gen InlineResponse20036
genInlineResponse20036 n =
  InlineResponse20036
    <$> arbitraryReducedMaybe n -- inlineResponse20036Data :: Maybe [WorkspaceCompact]
  
instance Arbitrary InlineResponse20037 where
  arbitrary = sized genInlineResponse20037

genInlineResponse20037 :: Int -> Gen InlineResponse20037
genInlineResponse20037 n =
  InlineResponse20037
    <$> arbitraryReducedMaybe n -- inlineResponse20037Data :: Maybe WorkspaceResponse
  
instance Arbitrary InlineResponse2004 where
  arbitrary = sized genInlineResponse2004

genInlineResponse2004 :: Int -> Gen InlineResponse2004
genInlineResponse2004 n =
  InlineResponse2004
    <$> arbitraryReducedMaybe n -- inlineResponse2004Data :: Maybe [CustomFieldSettingResponse]
  
instance Arbitrary InlineResponse2005 where
  arbitrary = sized genInlineResponse2005

genInlineResponse2005 :: Int -> Gen InlineResponse2005
genInlineResponse2005 n =
  InlineResponse2005
    <$> arbitraryReducedMaybe n -- inlineResponse2005Data :: Maybe [CustomFieldResponse]
  
instance Arbitrary InlineResponse2006 where
  arbitrary = sized genInlineResponse2006

genInlineResponse2006 :: Int -> Gen InlineResponse2006
genInlineResponse2006 n =
  InlineResponse2006
    <$> arbitraryReducedMaybe n -- inlineResponse2006Data :: Maybe [EventResponse]
    <*> arbitraryReducedMaybe n -- inlineResponse2006Sync :: Maybe Text
  
instance Arbitrary InlineResponse2007 where
  arbitrary = sized genInlineResponse2007

genInlineResponse2007 :: Int -> Gen InlineResponse2007
genInlineResponse2007 n =
  InlineResponse2007
    <$> arbitraryReducedMaybe n -- inlineResponse2007Data :: Maybe JobCompact
  
instance Arbitrary InlineResponse2008 where
  arbitrary = sized genInlineResponse2008

genInlineResponse2008 :: Int -> Gen InlineResponse2008
genInlineResponse2008 n =
  InlineResponse2008
    <$> arbitraryReducedMaybe n -- inlineResponse2008Data :: Maybe [PortfolioMembershipCompact]
  
instance Arbitrary InlineResponse2009 where
  arbitrary = sized genInlineResponse2009

genInlineResponse2009 :: Int -> Gen InlineResponse2009
genInlineResponse2009 n =
  InlineResponse2009
    <$> arbitraryReducedMaybe n -- inlineResponse2009Data :: Maybe PortfolioMembershipCompact
  
instance Arbitrary InlineResponse201 where
  arbitrary = sized genInlineResponse201

genInlineResponse201 :: Int -> Gen InlineResponse201
genInlineResponse201 n =
  InlineResponse201
    <$> arbitraryReducedMaybe n -- inlineResponse201Data :: Maybe CustomFieldResponse
  
instance Arbitrary InlineResponse2011 where
  arbitrary = sized genInlineResponse2011

genInlineResponse2011 :: Int -> Gen InlineResponse2011
genInlineResponse2011 n =
  InlineResponse2011
    <$> arbitraryReducedMaybe n -- inlineResponse2011Data :: Maybe EnumOption
  
instance Arbitrary InlineResponse2012 where
  arbitrary = sized genInlineResponse2012

genInlineResponse2012 :: Int -> Gen InlineResponse2012
genInlineResponse2012 n =
  InlineResponse2012
    <$> arbitraryReducedMaybe n -- inlineResponse2012Data :: Maybe OrganizationExportCompact
  
instance Arbitrary InlineResponse2013 where
  arbitrary = sized genInlineResponse2013

genInlineResponse2013 :: Int -> Gen InlineResponse2013
genInlineResponse2013 n =
  InlineResponse2013
    <$> arbitraryReducedMaybe n -- inlineResponse2013Data :: Maybe PortfolioResponse
  
instance Arbitrary InlineResponse2014 where
  arbitrary = sized genInlineResponse2014

genInlineResponse2014 :: Int -> Gen InlineResponse2014
genInlineResponse2014 n =
  InlineResponse2014
    <$> arbitraryReducedMaybe n -- inlineResponse2014Data :: Maybe ProjectResponse
  
instance Arbitrary InlineResponse2015 where
  arbitrary = sized genInlineResponse2015

genInlineResponse2015 :: Int -> Gen InlineResponse2015
genInlineResponse2015 n =
  InlineResponse2015
    <$> arbitraryReducedMaybe n -- inlineResponse2015Data :: Maybe TagResponse
  
instance Arbitrary InlineResponse2016 where
  arbitrary = sized genInlineResponse2016

genInlineResponse2016 :: Int -> Gen InlineResponse2016
genInlineResponse2016 n =
  InlineResponse2016
    <$> arbitraryReducedMaybe n -- inlineResponse2016Data :: Maybe TaskResponse
  
instance Arbitrary InlineResponse2017 where
  arbitrary = sized genInlineResponse2017

genInlineResponse2017 :: Int -> Gen InlineResponse2017
genInlineResponse2017 n =
  InlineResponse2017
    <$> arbitraryReducedMaybe n -- inlineResponse2017Data :: Maybe WebhookResponse
  
instance Arbitrary JobBase where
  arbitrary = sized genJobBase

genJobBase :: Int -> Gen JobBase
genJobBase n =
  
  pure JobBase
   
instance Arbitrary JobCompact where
  arbitrary = sized genJobCompact

genJobCompact :: Int -> Gen JobCompact
genJobCompact n =
  JobCompact
    <$> arbitraryReducedMaybe n -- jobCompactGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- jobCompactResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- jobCompactResourceSubtype :: Maybe Text
    <*> arbitraryReducedMaybe n -- jobCompactStatus :: Maybe E'Status
    <*> arbitraryReducedMaybe n -- jobCompactNewProject :: Maybe ProjectCompact
    <*> arbitraryReducedMaybe n -- jobCompactNewTask :: Maybe TaskCompact
  
instance Arbitrary JobCompactAllOf where
  arbitrary = sized genJobCompactAllOf

genJobCompactAllOf :: Int -> Gen JobCompactAllOf
genJobCompactAllOf n =
  JobCompactAllOf
    <$> arbitraryReducedMaybe n -- jobCompactAllOfResourceSubtype :: Maybe Text
    <*> arbitraryReducedMaybe n -- jobCompactAllOfStatus :: Maybe E'Status
    <*> arbitraryReducedMaybe n -- jobCompactAllOfNewProject :: Maybe ProjectCompact
    <*> arbitraryReducedMaybe n -- jobCompactAllOfNewTask :: Maybe TaskCompact
  
instance Arbitrary JobResponse where
  arbitrary = sized genJobResponse

genJobResponse :: Int -> Gen JobResponse
genJobResponse n =
  
  pure JobResponse
   
instance Arbitrary Like where
  arbitrary = sized genLike

genLike :: Int -> Gen Like
genLike n =
  Like
    <$> arbitraryReducedMaybe n -- likeGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- likeUser :: Maybe UserCompact
  
instance Arbitrary ModifyDependenciesRequest where
  arbitrary = sized genModifyDependenciesRequest

genModifyDependenciesRequest :: Int -> Gen ModifyDependenciesRequest
genModifyDependenciesRequest n =
  ModifyDependenciesRequest
    <$> arbitraryReducedMaybe n -- modifyDependenciesRequestDependencies :: Maybe [Text]
  
instance Arbitrary ModifyDependentsRequest where
  arbitrary = sized genModifyDependentsRequest

genModifyDependentsRequest :: Int -> Gen ModifyDependentsRequest
genModifyDependentsRequest n =
  ModifyDependentsRequest
    <$> arbitraryReducedMaybe n -- modifyDependentsRequestDependents :: Maybe [Text]
  
instance Arbitrary OrganizationExportBase where
  arbitrary = sized genOrganizationExportBase

genOrganizationExportBase :: Int -> Gen OrganizationExportBase
genOrganizationExportBase n =
  
  pure OrganizationExportBase
   
instance Arbitrary OrganizationExportCompact where
  arbitrary = sized genOrganizationExportCompact

genOrganizationExportCompact :: Int -> Gen OrganizationExportCompact
genOrganizationExportCompact n =
  OrganizationExportCompact
    <$> arbitraryReducedMaybe n -- organizationExportCompactGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- organizationExportCompactResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- organizationExportCompactCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- organizationExportCompactDownloadUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- organizationExportCompactState :: Maybe E'State
    <*> arbitraryReducedMaybe n -- organizationExportCompactOrganization :: Maybe WorkspaceCompact
  
instance Arbitrary OrganizationExportCompactAllOf where
  arbitrary = sized genOrganizationExportCompactAllOf

genOrganizationExportCompactAllOf :: Int -> Gen OrganizationExportCompactAllOf
genOrganizationExportCompactAllOf n =
  OrganizationExportCompactAllOf
    <$> arbitraryReducedMaybe n -- organizationExportCompactAllOfCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- organizationExportCompactAllOfDownloadUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- organizationExportCompactAllOfState :: Maybe E'State
    <*> arbitraryReducedMaybe n -- organizationExportCompactAllOfOrganization :: Maybe WorkspaceCompact
  
instance Arbitrary OrganizationExportRequest where
  arbitrary = sized genOrganizationExportRequest

genOrganizationExportRequest :: Int -> Gen OrganizationExportRequest
genOrganizationExportRequest n =
  OrganizationExportRequest
    <$> arbitraryReducedMaybe n -- organizationExportRequestOrganization :: Maybe Text
  
instance Arbitrary OrganizationExportResponse where
  arbitrary = sized genOrganizationExportResponse

genOrganizationExportResponse :: Int -> Gen OrganizationExportResponse
genOrganizationExportResponse n =
  
  pure OrganizationExportResponse
   
instance Arbitrary PortfolioAddItemRequest where
  arbitrary = sized genPortfolioAddItemRequest

genPortfolioAddItemRequest :: Int -> Gen PortfolioAddItemRequest
genPortfolioAddItemRequest n =
  PortfolioAddItemRequest
    <$> arbitrary -- portfolioAddItemRequestItem :: Text
    <*> arbitraryReducedMaybe n -- portfolioAddItemRequestInsertBefore :: Maybe Text
    <*> arbitraryReducedMaybe n -- portfolioAddItemRequestInsertAfter :: Maybe Text
  
instance Arbitrary PortfolioBase where
  arbitrary = sized genPortfolioBase

genPortfolioBase :: Int -> Gen PortfolioBase
genPortfolioBase n =
  PortfolioBase
    <$> arbitraryReducedMaybe n -- portfolioBaseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- portfolioBaseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- portfolioBaseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- portfolioBaseColor :: Maybe E'Color
  
instance Arbitrary PortfolioBaseAllOf where
  arbitrary = sized genPortfolioBaseAllOf

genPortfolioBaseAllOf :: Int -> Gen PortfolioBaseAllOf
genPortfolioBaseAllOf n =
  PortfolioBaseAllOf
    <$> arbitraryReducedMaybe n -- portfolioBaseAllOfColor :: Maybe E'Color
  
instance Arbitrary PortfolioCompact where
  arbitrary = sized genPortfolioCompact

genPortfolioCompact :: Int -> Gen PortfolioCompact
genPortfolioCompact n =
  PortfolioCompact
    <$> arbitraryReducedMaybe n -- portfolioCompactGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- portfolioCompactResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- portfolioCompactName :: Maybe Text
  
instance Arbitrary PortfolioCompactAllOf where
  arbitrary = sized genPortfolioCompactAllOf

genPortfolioCompactAllOf :: Int -> Gen PortfolioCompactAllOf
genPortfolioCompactAllOf n =
  PortfolioCompactAllOf
    <$> arbitraryReducedMaybe n -- portfolioCompactAllOfName :: Maybe Text
  
instance Arbitrary PortfolioMembershipBase where
  arbitrary = sized genPortfolioMembershipBase

genPortfolioMembershipBase :: Int -> Gen PortfolioMembershipBase
genPortfolioMembershipBase n =
  
  pure PortfolioMembershipBase
   
instance Arbitrary PortfolioMembershipCompact where
  arbitrary = sized genPortfolioMembershipCompact

genPortfolioMembershipCompact :: Int -> Gen PortfolioMembershipCompact
genPortfolioMembershipCompact n =
  PortfolioMembershipCompact
    <$> arbitraryReducedMaybe n -- portfolioMembershipCompactGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- portfolioMembershipCompactResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- portfolioMembershipCompactPortfolio :: Maybe PortfolioCompact
    <*> arbitraryReducedMaybe n -- portfolioMembershipCompactUser :: Maybe UserCompact
  
instance Arbitrary PortfolioMembershipCompactAllOf where
  arbitrary = sized genPortfolioMembershipCompactAllOf

genPortfolioMembershipCompactAllOf :: Int -> Gen PortfolioMembershipCompactAllOf
genPortfolioMembershipCompactAllOf n =
  PortfolioMembershipCompactAllOf
    <$> arbitraryReducedMaybe n -- portfolioMembershipCompactAllOfPortfolio :: Maybe PortfolioCompact
    <*> arbitraryReducedMaybe n -- portfolioMembershipCompactAllOfUser :: Maybe UserCompact
  
instance Arbitrary PortfolioMembershipResponse where
  arbitrary = sized genPortfolioMembershipResponse

genPortfolioMembershipResponse :: Int -> Gen PortfolioMembershipResponse
genPortfolioMembershipResponse n =
  
  pure PortfolioMembershipResponse
   
instance Arbitrary PortfolioRemoveItemRequest where
  arbitrary = sized genPortfolioRemoveItemRequest

genPortfolioRemoveItemRequest :: Int -> Gen PortfolioRemoveItemRequest
genPortfolioRemoveItemRequest n =
  PortfolioRemoveItemRequest
    <$> arbitrary -- portfolioRemoveItemRequestItem :: Text
  
instance Arbitrary PortfolioRequest where
  arbitrary = sized genPortfolioRequest

genPortfolioRequest :: Int -> Gen PortfolioRequest
genPortfolioRequest n =
  PortfolioRequest
    <$> arbitraryReducedMaybe n -- portfolioRequestGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- portfolioRequestResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- portfolioRequestName :: Maybe Text
    <*> arbitraryReducedMaybe n -- portfolioRequestColor :: Maybe E'Color
    <*> arbitraryReducedMaybe n -- portfolioRequestMembers :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- portfolioRequestWorkspace :: Maybe Text
  
instance Arbitrary PortfolioRequestAllOf where
  arbitrary = sized genPortfolioRequestAllOf

genPortfolioRequestAllOf :: Int -> Gen PortfolioRequestAllOf
genPortfolioRequestAllOf n =
  PortfolioRequestAllOf
    <$> arbitraryReducedMaybe n -- portfolioRequestAllOfMembers :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- portfolioRequestAllOfWorkspace :: Maybe Text
  
instance Arbitrary PortfolioResponse where
  arbitrary = sized genPortfolioResponse

genPortfolioResponse :: Int -> Gen PortfolioResponse
genPortfolioResponse n =
  PortfolioResponse
    <$> arbitraryReducedMaybe n -- portfolioResponseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- portfolioResponseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- portfolioResponseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- portfolioResponseColor :: Maybe E'Color
    <*> arbitraryReducedMaybe n -- portfolioResponseCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- portfolioResponseCreatedBy :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- portfolioResponseCustomFieldSettings :: Maybe [CustomFieldSettingResponse]
    <*> arbitraryReducedMaybe n -- portfolioResponseMembers :: Maybe [UserCompact]
    <*> arbitraryReducedMaybe n -- portfolioResponseOwner :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- portfolioResponseWorkspace :: Maybe WorkspaceCompact
  
instance Arbitrary PortfolioResponseAllOf where
  arbitrary = sized genPortfolioResponseAllOf

genPortfolioResponseAllOf :: Int -> Gen PortfolioResponseAllOf
genPortfolioResponseAllOf n =
  PortfolioResponseAllOf
    <$> arbitraryReducedMaybe n -- portfolioResponseAllOfCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- portfolioResponseAllOfCreatedBy :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- portfolioResponseAllOfCustomFieldSettings :: Maybe [CustomFieldSettingResponse]
    <*> arbitraryReducedMaybe n -- portfolioResponseAllOfMembers :: Maybe [UserCompact]
    <*> arbitraryReducedMaybe n -- portfolioResponseAllOfOwner :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- portfolioResponseAllOfWorkspace :: Maybe WorkspaceCompact
  
instance Arbitrary Preview where
  arbitrary = sized genPreview

genPreview :: Int -> Gen Preview
genPreview n =
  Preview
    <$> arbitraryReducedMaybe n -- previewFallback :: Maybe Text
    <*> arbitraryReducedMaybe n -- previewFooter :: Maybe Text
    <*> arbitraryReducedMaybe n -- previewHeader :: Maybe Text
    <*> arbitraryReducedMaybe n -- previewHeaderLink :: Maybe Text
    <*> arbitraryReducedMaybe n -- previewHtmlText :: Maybe Text
    <*> arbitraryReducedMaybe n -- previewText :: Maybe Text
    <*> arbitraryReducedMaybe n -- previewTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- previewTitleLink :: Maybe Text
  
instance Arbitrary ProjectBase where
  arbitrary = sized genProjectBase

genProjectBase :: Int -> Gen ProjectBase
genProjectBase n =
  ProjectBase
    <$> arbitraryReducedMaybe n -- projectBaseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectBaseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectBaseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectBaseArchived :: Maybe Bool
    <*> arbitraryReducedMaybe n -- projectBaseColor :: Maybe E'Color
    <*> arbitraryReducedMaybe n -- projectBaseCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- projectBaseCurrentStatus :: Maybe ProjectStatusResponse
    <*> arbitraryReducedMaybe n -- projectBaseCustomFieldSettings :: Maybe [CustomFieldSettingCompact]
    <*> arbitraryReducedMaybe n -- projectBaseDefaultView :: Maybe E'DefaultView
    <*> arbitraryReducedMaybe n -- projectBaseDueDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- projectBaseDueOn :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- projectBaseHtmlNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectBaseIsTemplate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- projectBaseMembers :: Maybe [UserCompact]
    <*> arbitraryReducedMaybe n -- projectBaseModifiedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- projectBaseNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectBasePublic :: Maybe Bool
    <*> arbitraryReducedMaybe n -- projectBaseStartOn :: Maybe Date
    <*> arbitraryReducedMaybe n -- projectBaseWorkspace :: Maybe WorkspaceCompact
  
instance Arbitrary ProjectBaseAllOf where
  arbitrary = sized genProjectBaseAllOf

genProjectBaseAllOf :: Int -> Gen ProjectBaseAllOf
genProjectBaseAllOf n =
  ProjectBaseAllOf
    <$> arbitraryReducedMaybe n -- projectBaseAllOfArchived :: Maybe Bool
    <*> arbitraryReducedMaybe n -- projectBaseAllOfColor :: Maybe E'Color
    <*> arbitraryReducedMaybe n -- projectBaseAllOfCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- projectBaseAllOfCurrentStatus :: Maybe ProjectStatusResponse
    <*> arbitraryReducedMaybe n -- projectBaseAllOfCustomFieldSettings :: Maybe [CustomFieldSettingCompact]
    <*> arbitraryReducedMaybe n -- projectBaseAllOfDefaultView :: Maybe E'DefaultView
    <*> arbitraryReducedMaybe n -- projectBaseAllOfDueDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- projectBaseAllOfDueOn :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- projectBaseAllOfHtmlNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectBaseAllOfIsTemplate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- projectBaseAllOfMembers :: Maybe [UserCompact]
    <*> arbitraryReducedMaybe n -- projectBaseAllOfModifiedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- projectBaseAllOfNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectBaseAllOfPublic :: Maybe Bool
    <*> arbitraryReducedMaybe n -- projectBaseAllOfStartOn :: Maybe Date
    <*> arbitraryReducedMaybe n -- projectBaseAllOfWorkspace :: Maybe WorkspaceCompact
  
instance Arbitrary ProjectCompact where
  arbitrary = sized genProjectCompact

genProjectCompact :: Int -> Gen ProjectCompact
genProjectCompact n =
  ProjectCompact
    <$> arbitraryReducedMaybe n -- projectCompactGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectCompactResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectCompactName :: Maybe Text
  
instance Arbitrary ProjectCompactAllOf where
  arbitrary = sized genProjectCompactAllOf

genProjectCompactAllOf :: Int -> Gen ProjectCompactAllOf
genProjectCompactAllOf n =
  ProjectCompactAllOf
    <$> arbitraryReducedMaybe n -- projectCompactAllOfName :: Maybe Text
  
instance Arbitrary ProjectDuplicateRequest where
  arbitrary = sized genProjectDuplicateRequest

genProjectDuplicateRequest :: Int -> Gen ProjectDuplicateRequest
genProjectDuplicateRequest n =
  ProjectDuplicateRequest
    <$> arbitrary -- projectDuplicateRequestName :: Text
    <*> arbitraryReducedMaybe n -- projectDuplicateRequestTeam :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectDuplicateRequestInclude :: Maybe E'Include
    <*> arbitraryReducedMaybe n -- projectDuplicateRequestScheduleDates :: Maybe ProjectDuplicateRequestScheduleDates
  
instance Arbitrary ProjectDuplicateRequestScheduleDates where
  arbitrary = sized genProjectDuplicateRequestScheduleDates

genProjectDuplicateRequestScheduleDates :: Int -> Gen ProjectDuplicateRequestScheduleDates
genProjectDuplicateRequestScheduleDates n =
  ProjectDuplicateRequestScheduleDates
    <$> arbitrary -- projectDuplicateRequestScheduleDatesShouldSkipWeekends :: Bool
    <*> arbitraryReducedMaybe n -- projectDuplicateRequestScheduleDatesDueOn :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectDuplicateRequestScheduleDatesStartOn :: Maybe Text
  
instance Arbitrary ProjectMembershipBase where
  arbitrary = sized genProjectMembershipBase

genProjectMembershipBase :: Int -> Gen ProjectMembershipBase
genProjectMembershipBase n =
  
  pure ProjectMembershipBase
   
instance Arbitrary ProjectMembershipCompact where
  arbitrary = sized genProjectMembershipCompact

genProjectMembershipCompact :: Int -> Gen ProjectMembershipCompact
genProjectMembershipCompact n =
  ProjectMembershipCompact
    <$> arbitraryReducedMaybe n -- projectMembershipCompactGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectMembershipCompactResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectMembershipCompactUser :: Maybe UserCompact
  
instance Arbitrary ProjectMembershipCompactAllOf where
  arbitrary = sized genProjectMembershipCompactAllOf

genProjectMembershipCompactAllOf :: Int -> Gen ProjectMembershipCompactAllOf
genProjectMembershipCompactAllOf n =
  ProjectMembershipCompactAllOf
    <$> arbitraryReducedMaybe n -- projectMembershipCompactAllOfUser :: Maybe UserCompact
  
instance Arbitrary ProjectMembershipResponse where
  arbitrary = sized genProjectMembershipResponse

genProjectMembershipResponse :: Int -> Gen ProjectMembershipResponse
genProjectMembershipResponse n =
  ProjectMembershipResponse
    <$> arbitraryReducedMaybe n -- projectMembershipResponseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectMembershipResponseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectMembershipResponseUser :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- projectMembershipResponseProject :: Maybe ProjectCompact
    <*> arbitraryReducedMaybe n -- projectMembershipResponseWriteAccess :: Maybe E'WriteAccess
  
instance Arbitrary ProjectMembershipResponseAllOf where
  arbitrary = sized genProjectMembershipResponseAllOf

genProjectMembershipResponseAllOf :: Int -> Gen ProjectMembershipResponseAllOf
genProjectMembershipResponseAllOf n =
  ProjectMembershipResponseAllOf
    <$> arbitraryReducedMaybe n -- projectMembershipResponseAllOfProject :: Maybe ProjectCompact
    <*> arbitraryReducedMaybe n -- projectMembershipResponseAllOfWriteAccess :: Maybe E'WriteAccess
  
instance Arbitrary ProjectRequest where
  arbitrary = sized genProjectRequest

genProjectRequest :: Int -> Gen ProjectRequest
genProjectRequest n =
  ProjectRequest
    <$> arbitraryReducedMaybe n -- projectRequestGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectRequestResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectRequestName :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectRequestArchived :: Maybe Bool
    <*> arbitraryReducedMaybe n -- projectRequestColor :: Maybe E'Color
    <*> arbitraryReducedMaybe n -- projectRequestCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- projectRequestCurrentStatus :: Maybe ProjectStatusResponse
    <*> arbitraryReducedMaybe n -- projectRequestCustomFieldSettings :: Maybe [CustomFieldSettingCompact]
    <*> arbitraryReducedMaybe n -- projectRequestDefaultView :: Maybe E'DefaultView
    <*> arbitraryReducedMaybe n -- projectRequestDueDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- projectRequestDueOn :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- projectRequestHtmlNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectRequestIsTemplate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- projectRequestMembers :: Maybe [UserCompact]
    <*> arbitraryReducedMaybe n -- projectRequestModifiedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- projectRequestNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectRequestPublic :: Maybe Bool
    <*> arbitraryReducedMaybe n -- projectRequestStartOn :: Maybe Date
    <*> arbitraryReducedMaybe n -- projectRequestWorkspace :: Maybe WorkspaceCompact
    <*> arbitraryReducedMaybe n -- projectRequestCustomFields :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- projectRequestFollowers :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectRequestOwner :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectRequestTeam :: Maybe Text
  
instance Arbitrary ProjectRequestAllOf where
  arbitrary = sized genProjectRequestAllOf

genProjectRequestAllOf :: Int -> Gen ProjectRequestAllOf
genProjectRequestAllOf n =
  ProjectRequestAllOf
    <$> arbitraryReducedMaybe n -- projectRequestAllOfCustomFields :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- projectRequestAllOfFollowers :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectRequestAllOfOwner :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectRequestAllOfTeam :: Maybe Text
  
instance Arbitrary ProjectResponse where
  arbitrary = sized genProjectResponse

genProjectResponse :: Int -> Gen ProjectResponse
genProjectResponse n =
  ProjectResponse
    <$> arbitraryReducedMaybe n -- projectResponseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectResponseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectResponseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectResponseArchived :: Maybe Bool
    <*> arbitraryReducedMaybe n -- projectResponseColor :: Maybe E'Color
    <*> arbitraryReducedMaybe n -- projectResponseCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- projectResponseCurrentStatus :: Maybe ProjectStatusResponse
    <*> arbitraryReducedMaybe n -- projectResponseCustomFieldSettings :: Maybe [CustomFieldSettingCompact]
    <*> arbitraryReducedMaybe n -- projectResponseDefaultView :: Maybe E'DefaultView
    <*> arbitraryReducedMaybe n -- projectResponseDueDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- projectResponseDueOn :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- projectResponseHtmlNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectResponseIsTemplate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- projectResponseMembers :: Maybe [UserCompact]
    <*> arbitraryReducedMaybe n -- projectResponseModifiedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- projectResponseNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectResponsePublic :: Maybe Bool
    <*> arbitraryReducedMaybe n -- projectResponseStartOn :: Maybe Date
    <*> arbitraryReducedMaybe n -- projectResponseWorkspace :: Maybe WorkspaceCompact
    <*> arbitraryReducedMaybe n -- projectResponseCustomFields :: Maybe [CustomFieldCompact]
    <*> arbitraryReducedMaybe n -- projectResponseFollowers :: Maybe [UserCompact]
    <*> arbitraryReducedMaybe n -- projectResponseOwner :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- projectResponseTeam :: Maybe TeamCompact
  
instance Arbitrary ProjectResponseAllOf where
  arbitrary = sized genProjectResponseAllOf

genProjectResponseAllOf :: Int -> Gen ProjectResponseAllOf
genProjectResponseAllOf n =
  ProjectResponseAllOf
    <$> arbitraryReducedMaybe n -- projectResponseAllOfCustomFields :: Maybe [CustomFieldCompact]
    <*> arbitraryReducedMaybe n -- projectResponseAllOfFollowers :: Maybe [UserCompact]
    <*> arbitraryReducedMaybe n -- projectResponseAllOfOwner :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- projectResponseAllOfTeam :: Maybe TeamCompact
  
instance Arbitrary ProjectSectionInsertRequest where
  arbitrary = sized genProjectSectionInsertRequest

genProjectSectionInsertRequest :: Int -> Gen ProjectSectionInsertRequest
genProjectSectionInsertRequest n =
  ProjectSectionInsertRequest
    <$> arbitrary -- projectSectionInsertRequestProject :: Text
    <*> arbitrary -- projectSectionInsertRequestSection :: Text
    <*> arbitraryReducedMaybe n -- projectSectionInsertRequestBeforeSection :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectSectionInsertRequestAfterSection :: Maybe Text
  
instance Arbitrary ProjectStatusBase where
  arbitrary = sized genProjectStatusBase

genProjectStatusBase :: Int -> Gen ProjectStatusBase
genProjectStatusBase n =
  ProjectStatusBase
    <$> arbitraryReducedMaybe n -- projectStatusBaseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectStatusBaseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectStatusBaseTitle :: Maybe Text
    <*> arbitrary -- projectStatusBaseProject :: Text
    <*> arbitrary -- projectStatusBaseText :: Text
    <*> arbitraryReducedMaybe n -- projectStatusBaseHtmlText :: Maybe Text
    <*> arbitrary -- projectStatusBaseColor :: E'Color2
  
instance Arbitrary ProjectStatusBaseAllOf where
  arbitrary = sized genProjectStatusBaseAllOf

genProjectStatusBaseAllOf :: Int -> Gen ProjectStatusBaseAllOf
genProjectStatusBaseAllOf n =
  ProjectStatusBaseAllOf
    <$> arbitrary -- projectStatusBaseAllOfProject :: Text
    <*> arbitrary -- projectStatusBaseAllOfText :: Text
    <*> arbitraryReducedMaybe n -- projectStatusBaseAllOfHtmlText :: Maybe Text
    <*> arbitrary -- projectStatusBaseAllOfColor :: E'Color2
  
instance Arbitrary ProjectStatusCompact where
  arbitrary = sized genProjectStatusCompact

genProjectStatusCompact :: Int -> Gen ProjectStatusCompact
genProjectStatusCompact n =
  ProjectStatusCompact
    <$> arbitraryReducedMaybe n -- projectStatusCompactGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectStatusCompactResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectStatusCompactTitle :: Maybe Text
  
instance Arbitrary ProjectStatusCompactAllOf where
  arbitrary = sized genProjectStatusCompactAllOf

genProjectStatusCompactAllOf :: Int -> Gen ProjectStatusCompactAllOf
genProjectStatusCompactAllOf n =
  ProjectStatusCompactAllOf
    <$> arbitraryReducedMaybe n -- projectStatusCompactAllOfTitle :: Maybe Text
  
instance Arbitrary ProjectStatusRequest where
  arbitrary = sized genProjectStatusRequest

genProjectStatusRequest :: Int -> Gen ProjectStatusRequest
genProjectStatusRequest n =
  
  pure ProjectStatusRequest
   
instance Arbitrary ProjectStatusResponse where
  arbitrary = sized genProjectStatusResponse

genProjectStatusResponse :: Int -> Gen ProjectStatusResponse
genProjectStatusResponse n =
  ProjectStatusResponse
    <$> arbitraryReducedMaybe n -- projectStatusResponseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectStatusResponseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectStatusResponseTitle :: Maybe Text
    <*> arbitrary -- projectStatusResponseProject :: Text
    <*> arbitrary -- projectStatusResponseText :: Text
    <*> arbitraryReducedMaybe n -- projectStatusResponseHtmlText :: Maybe Text
    <*> arbitrary -- projectStatusResponseColor :: E'Color2
    <*> arbitraryReducedMaybe n -- projectStatusResponseCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- projectStatusResponseCreatedBy :: Maybe UserCompact
  
instance Arbitrary ProjectStatusResponseAllOf where
  arbitrary = sized genProjectStatusResponseAllOf

genProjectStatusResponseAllOf :: Int -> Gen ProjectStatusResponseAllOf
genProjectStatusResponseAllOf n =
  ProjectStatusResponseAllOf
    <$> arbitraryReducedMaybe n -- projectStatusResponseAllOfCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- projectStatusResponseAllOfCreatedBy :: Maybe UserCompact
  
instance Arbitrary RemoveCustomFieldSettingRequest where
  arbitrary = sized genRemoveCustomFieldSettingRequest

genRemoveCustomFieldSettingRequest :: Int -> Gen RemoveCustomFieldSettingRequest
genRemoveCustomFieldSettingRequest n =
  RemoveCustomFieldSettingRequest
    <$> arbitrary -- removeCustomFieldSettingRequestCustomField :: Text
  
instance Arbitrary RemoveFollowersRequest where
  arbitrary = sized genRemoveFollowersRequest

genRemoveFollowersRequest :: Int -> Gen RemoveFollowersRequest
genRemoveFollowersRequest n =
  RemoveFollowersRequest
    <$> arbitrary -- removeFollowersRequestFollowers :: Text
  
instance Arbitrary RemoveMembersRequest where
  arbitrary = sized genRemoveMembersRequest

genRemoveMembersRequest :: Int -> Gen RemoveMembersRequest
genRemoveMembersRequest n =
  RemoveMembersRequest
    <$> arbitrary -- removeMembersRequestMembers :: Text
  
instance Arbitrary SectionBase where
  arbitrary = sized genSectionBase

genSectionBase :: Int -> Gen SectionBase
genSectionBase n =
  
  pure SectionBase
   
instance Arbitrary SectionCompact where
  arbitrary = sized genSectionCompact

genSectionCompact :: Int -> Gen SectionCompact
genSectionCompact n =
  SectionCompact
    <$> arbitraryReducedMaybe n -- sectionCompactGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- sectionCompactResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- sectionCompactName :: Maybe Text
  
instance Arbitrary SectionCompactAllOf where
  arbitrary = sized genSectionCompactAllOf

genSectionCompactAllOf :: Int -> Gen SectionCompactAllOf
genSectionCompactAllOf n =
  SectionCompactAllOf
    <$> arbitraryReducedMaybe n -- sectionCompactAllOfName :: Maybe Text
  
instance Arbitrary SectionRequest where
  arbitrary = sized genSectionRequest

genSectionRequest :: Int -> Gen SectionRequest
genSectionRequest n =
  SectionRequest
    <$> arbitrary -- sectionRequestProject :: Text
    <*> arbitrary -- sectionRequestName :: Text
  
instance Arbitrary SectionResponse where
  arbitrary = sized genSectionResponse

genSectionResponse :: Int -> Gen SectionResponse
genSectionResponse n =
  SectionResponse
    <$> arbitraryReducedMaybe n -- sectionResponseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- sectionResponseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- sectionResponseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- sectionResponseCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- sectionResponseProject :: Maybe ProjectCompact
    <*> arbitraryReducedMaybe n -- sectionResponseProjects :: Maybe [ProjectCompact]
  
instance Arbitrary SectionResponseAllOf where
  arbitrary = sized genSectionResponseAllOf

genSectionResponseAllOf :: Int -> Gen SectionResponseAllOf
genSectionResponseAllOf n =
  SectionResponseAllOf
    <$> arbitraryReducedMaybe n -- sectionResponseAllOfCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- sectionResponseAllOfProject :: Maybe ProjectCompact
    <*> arbitraryReducedMaybe n -- sectionResponseAllOfProjects :: Maybe [ProjectCompact]
  
instance Arbitrary SectionTaskInsertRequest where
  arbitrary = sized genSectionTaskInsertRequest

genSectionTaskInsertRequest :: Int -> Gen SectionTaskInsertRequest
genSectionTaskInsertRequest n =
  SectionTaskInsertRequest
    <$> arbitrary -- sectionTaskInsertRequestTask :: Text
    <*> arbitraryReducedMaybe n -- sectionTaskInsertRequestInsertBefore :: Maybe Text
    <*> arbitraryReducedMaybe n -- sectionTaskInsertRequestInsertAfter :: Maybe Text
  
instance Arbitrary StoryBase where
  arbitrary = sized genStoryBase

genStoryBase :: Int -> Gen StoryBase
genStoryBase n =
  StoryBase
    <$> arbitraryReducedMaybe n -- storyBaseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyBaseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyBaseCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- storyBaseCreatedBy :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- storyBaseResourceSubtype :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyBaseText :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyBaseType :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyBaseHtmlText :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyBaseIsPinned :: Maybe Bool
  
instance Arbitrary StoryBaseAllOf where
  arbitrary = sized genStoryBaseAllOf

genStoryBaseAllOf :: Int -> Gen StoryBaseAllOf
genStoryBaseAllOf n =
  StoryBaseAllOf
    <$> arbitraryReducedMaybe n -- storyBaseAllOfText :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyBaseAllOfHtmlText :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyBaseAllOfIsPinned :: Maybe Bool
  
instance Arbitrary StoryCompact where
  arbitrary = sized genStoryCompact

genStoryCompact :: Int -> Gen StoryCompact
genStoryCompact n =
  StoryCompact
    <$> arbitraryReducedMaybe n -- storyCompactGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyCompactResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyCompactCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- storyCompactCreatedBy :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- storyCompactResourceSubtype :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyCompactText :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyCompactType :: Maybe Text
  
instance Arbitrary StoryCompactAllOf where
  arbitrary = sized genStoryCompactAllOf

genStoryCompactAllOf :: Int -> Gen StoryCompactAllOf
genStoryCompactAllOf n =
  StoryCompactAllOf
    <$> arbitraryReducedMaybe n -- storyCompactAllOfCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- storyCompactAllOfCreatedBy :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- storyCompactAllOfResourceSubtype :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyCompactAllOfText :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyCompactAllOfType :: Maybe Text
  
instance Arbitrary StoryRequest where
  arbitrary = sized genStoryRequest

genStoryRequest :: Int -> Gen StoryRequest
genStoryRequest n =
  
  pure StoryRequest
   
instance Arbitrary StoryResponse where
  arbitrary = sized genStoryResponse

genStoryResponse :: Int -> Gen StoryResponse
genStoryResponse n =
  StoryResponse
    <$> arbitraryReducedMaybe n -- storyResponseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- storyResponseCreatedBy :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- storyResponseResourceSubtype :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseText :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseType :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseHtmlText :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseIsPinned :: Maybe Bool
    <*> arbitraryReducedMaybe n -- storyResponseIsEdited :: Maybe Bool
    <*> arbitraryReducedMaybe n -- storyResponseHearted :: Maybe Bool
    <*> arbitraryReducedMaybe n -- storyResponseHearts :: Maybe [Like]
    <*> arbitraryReducedMaybe n -- storyResponseNumHearts :: Maybe Int
    <*> arbitraryReducedMaybe n -- storyResponseLiked :: Maybe Bool
    <*> arbitraryReducedMaybe n -- storyResponseLikes :: Maybe [Like]
    <*> arbitraryReducedMaybe n -- storyResponseNumLikes :: Maybe Int
    <*> arbitraryReducedMaybe n -- storyResponsePreviews :: Maybe [Preview]
    <*> arbitraryReducedMaybe n -- storyResponseOldName :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseNewName :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseOldDates :: Maybe StoryResponseDates
    <*> arbitraryReducedMaybe n -- storyResponseNewDates :: Maybe StoryResponseDates
    <*> arbitraryReducedMaybe n -- storyResponseOldResourceSubtype :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseNewResourceSubtype :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseStory :: Maybe StoryCompact
    <*> arbitraryReducedMaybe n -- storyResponseAssignee :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- storyResponseFollower :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- storyResponseOldSection :: Maybe SectionCompact
    <*> arbitraryReducedMaybe n -- storyResponseNewSection :: Maybe SectionCompact
    <*> arbitraryReducedMaybe n -- storyResponseTask :: Maybe TaskCompact
    <*> arbitraryReducedMaybe n -- storyResponseProject :: Maybe ProjectCompact
    <*> arbitraryReducedMaybe n -- storyResponseTag :: Maybe TagCompact
    <*> arbitraryReducedMaybe n -- storyResponseCustomField :: Maybe CustomFieldCompact
    <*> arbitraryReducedMaybe n -- storyResponseOldTextValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseNewTextValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseOldNumberValue :: Maybe Int
    <*> arbitraryReducedMaybe n -- storyResponseNewNumberValue :: Maybe Int
    <*> arbitraryReducedMaybe n -- storyResponseOldEnumValue :: Maybe EnumOption
    <*> arbitraryReducedMaybe n -- storyResponseNewEnumValue :: Maybe EnumOption
    <*> arbitraryReducedMaybe n -- storyResponseNewApprovalStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseOldApprovalStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseDuplicateOf :: Maybe TaskCompact
    <*> arbitraryReducedMaybe n -- storyResponseDuplicatedFrom :: Maybe TaskCompact
    <*> arbitraryReducedMaybe n -- storyResponseDependency :: Maybe TaskCompact
    <*> arbitraryReducedMaybe n -- storyResponseSource :: Maybe E'Source
    <*> arbitraryReducedMaybeValue n -- storyResponseTarget :: Maybe A.Value
  
instance Arbitrary StoryResponseAllOf where
  arbitrary = sized genStoryResponseAllOf

genStoryResponseAllOf :: Int -> Gen StoryResponseAllOf
genStoryResponseAllOf n =
  StoryResponseAllOf
    <$> arbitraryReducedMaybe n -- storyResponseAllOfIsEdited :: Maybe Bool
    <*> arbitraryReducedMaybe n -- storyResponseAllOfHearted :: Maybe Bool
    <*> arbitraryReducedMaybe n -- storyResponseAllOfHearts :: Maybe [Like]
    <*> arbitraryReducedMaybe n -- storyResponseAllOfNumHearts :: Maybe Int
    <*> arbitraryReducedMaybe n -- storyResponseAllOfLiked :: Maybe Bool
    <*> arbitraryReducedMaybe n -- storyResponseAllOfLikes :: Maybe [Like]
    <*> arbitraryReducedMaybe n -- storyResponseAllOfNumLikes :: Maybe Int
    <*> arbitraryReducedMaybe n -- storyResponseAllOfPreviews :: Maybe [Preview]
    <*> arbitraryReducedMaybe n -- storyResponseAllOfOldName :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseAllOfNewName :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseAllOfOldDates :: Maybe StoryResponseDates
    <*> arbitraryReducedMaybe n -- storyResponseAllOfNewDates :: Maybe StoryResponseDates
    <*> arbitraryReducedMaybe n -- storyResponseAllOfOldResourceSubtype :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseAllOfNewResourceSubtype :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseAllOfStory :: Maybe StoryCompact
    <*> arbitraryReducedMaybe n -- storyResponseAllOfAssignee :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- storyResponseAllOfFollower :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- storyResponseAllOfOldSection :: Maybe SectionCompact
    <*> arbitraryReducedMaybe n -- storyResponseAllOfNewSection :: Maybe SectionCompact
    <*> arbitraryReducedMaybe n -- storyResponseAllOfTask :: Maybe TaskCompact
    <*> arbitraryReducedMaybe n -- storyResponseAllOfProject :: Maybe ProjectCompact
    <*> arbitraryReducedMaybe n -- storyResponseAllOfTag :: Maybe TagCompact
    <*> arbitraryReducedMaybe n -- storyResponseAllOfCustomField :: Maybe CustomFieldCompact
    <*> arbitraryReducedMaybe n -- storyResponseAllOfOldTextValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseAllOfNewTextValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseAllOfOldNumberValue :: Maybe Int
    <*> arbitraryReducedMaybe n -- storyResponseAllOfNewNumberValue :: Maybe Int
    <*> arbitraryReducedMaybe n -- storyResponseAllOfOldEnumValue :: Maybe EnumOption
    <*> arbitraryReducedMaybe n -- storyResponseAllOfNewEnumValue :: Maybe EnumOption
    <*> arbitraryReducedMaybe n -- storyResponseAllOfNewApprovalStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseAllOfOldApprovalStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- storyResponseAllOfDuplicateOf :: Maybe TaskCompact
    <*> arbitraryReducedMaybe n -- storyResponseAllOfDuplicatedFrom :: Maybe TaskCompact
    <*> arbitraryReducedMaybe n -- storyResponseAllOfDependency :: Maybe TaskCompact
    <*> arbitraryReducedMaybe n -- storyResponseAllOfSource :: Maybe E'Source
    <*> arbitraryReducedMaybeValue n -- storyResponseAllOfTarget :: Maybe A.Value
  
instance Arbitrary StoryResponseDates where
  arbitrary = sized genStoryResponseDates

genStoryResponseDates :: Int -> Gen StoryResponseDates
genStoryResponseDates n =
  StoryResponseDates
    <$> arbitraryReducedMaybe n -- storyResponseDatesStartOn :: Maybe Date
    <*> arbitraryReducedMaybe n -- storyResponseDatesDueAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- storyResponseDatesDueOn :: Maybe Date
  
instance Arbitrary TagBase where
  arbitrary = sized genTagBase

genTagBase :: Int -> Gen TagBase
genTagBase n =
  TagBase
    <$> arbitraryReducedMaybe n -- tagBaseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- tagBaseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- tagBaseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- tagBaseColor :: Maybe E'Color
  
instance Arbitrary TagBaseAllOf where
  arbitrary = sized genTagBaseAllOf

genTagBaseAllOf :: Int -> Gen TagBaseAllOf
genTagBaseAllOf n =
  TagBaseAllOf
    <$> arbitraryReducedMaybe n -- tagBaseAllOfColor :: Maybe E'Color
  
instance Arbitrary TagCompact where
  arbitrary = sized genTagCompact

genTagCompact :: Int -> Gen TagCompact
genTagCompact n =
  TagCompact
    <$> arbitraryReducedMaybe n -- tagCompactGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- tagCompactResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- tagCompactName :: Maybe Text
  
instance Arbitrary TagCompactAllOf where
  arbitrary = sized genTagCompactAllOf

genTagCompactAllOf :: Int -> Gen TagCompactAllOf
genTagCompactAllOf n =
  TagCompactAllOf
    <$> arbitraryReducedMaybe n -- tagCompactAllOfName :: Maybe Text
  
instance Arbitrary TagRequest where
  arbitrary = sized genTagRequest

genTagRequest :: Int -> Gen TagRequest
genTagRequest n =
  TagRequest
    <$> arbitraryReducedMaybe n -- tagRequestGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- tagRequestResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- tagRequestName :: Maybe Text
    <*> arbitraryReducedMaybe n -- tagRequestColor :: Maybe E'Color
    <*> arbitraryReducedMaybe n -- tagRequestFollowers :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- tagRequestWorkspace :: Maybe Text
  
instance Arbitrary TagRequestAllOf where
  arbitrary = sized genTagRequestAllOf

genTagRequestAllOf :: Int -> Gen TagRequestAllOf
genTagRequestAllOf n =
  TagRequestAllOf
    <$> arbitraryReducedMaybe n -- tagRequestAllOfFollowers :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- tagRequestAllOfWorkspace :: Maybe Text
  
instance Arbitrary TagResponse where
  arbitrary = sized genTagResponse

genTagResponse :: Int -> Gen TagResponse
genTagResponse n =
  TagResponse
    <$> arbitraryReducedMaybe n -- tagResponseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- tagResponseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- tagResponseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- tagResponseColor :: Maybe E'Color
    <*> arbitraryReducedMaybe n -- tagResponseFollowers :: Maybe [UserCompact]
    <*> arbitraryReducedMaybe n -- tagResponseWorkspace :: Maybe WorkspaceCompact
  
instance Arbitrary TagResponseAllOf where
  arbitrary = sized genTagResponseAllOf

genTagResponseAllOf :: Int -> Gen TagResponseAllOf
genTagResponseAllOf n =
  TagResponseAllOf
    <$> arbitraryReducedMaybe n -- tagResponseAllOfFollowers :: Maybe [UserCompact]
    <*> arbitraryReducedMaybe n -- tagResponseAllOfWorkspace :: Maybe WorkspaceCompact
  
instance Arbitrary TaskAddFollowersRequest where
  arbitrary = sized genTaskAddFollowersRequest

genTaskAddFollowersRequest :: Int -> Gen TaskAddFollowersRequest
genTaskAddFollowersRequest n =
  TaskAddFollowersRequest
    <$> arbitrary -- taskAddFollowersRequestFollowers :: [Text]
  
instance Arbitrary TaskAddProjectRequest where
  arbitrary = sized genTaskAddProjectRequest

genTaskAddProjectRequest :: Int -> Gen TaskAddProjectRequest
genTaskAddProjectRequest n =
  TaskAddProjectRequest
    <$> arbitrary -- taskAddProjectRequestProject :: Text
    <*> arbitraryReducedMaybe n -- taskAddProjectRequestInsertAfter :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskAddProjectRequestInsertBefore :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskAddProjectRequestSection :: Maybe Text
  
instance Arbitrary TaskAddTagRequest where
  arbitrary = sized genTaskAddTagRequest

genTaskAddTagRequest :: Int -> Gen TaskAddTagRequest
genTaskAddTagRequest n =
  TaskAddTagRequest
    <$> arbitrary -- taskAddTagRequestTag :: Text
  
instance Arbitrary TaskBase where
  arbitrary = sized genTaskBase

genTaskBase :: Int -> Gen TaskBase
genTaskBase n =
  TaskBase
    <$> arbitraryReducedMaybe n -- taskBaseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskBaseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskBaseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskBaseApprovalStatus :: Maybe E'ApprovalStatus
    <*> arbitraryReducedMaybe n -- taskBaseAssigneeStatus :: Maybe E'AssigneeStatus
    <*> arbitraryReducedMaybe n -- taskBaseCompleted :: Maybe Bool
    <*> arbitraryReducedMaybe n -- taskBaseCompletedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- taskBaseCompletedBy :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- taskBaseCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- taskBaseDependencies :: Maybe [AsanaResource]
    <*> arbitraryReducedMaybe n -- taskBaseDependents :: Maybe [AsanaResource]
    <*> arbitraryReducedMaybe n -- taskBaseDueAt :: Maybe Date
    <*> arbitraryReducedMaybe n -- taskBaseDueOn :: Maybe Date
    <*> arbitraryReducedMaybe n -- taskBaseExternal :: Maybe TaskBaseAllOfExternal
    <*> arbitraryReducedMaybe n -- taskBaseHtmlNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskBaseHearted :: Maybe Bool
    <*> arbitraryReducedMaybe n -- taskBaseHearts :: Maybe [Like]
    <*> arbitraryReducedMaybe n -- taskBaseIsRenderedAsSeparator :: Maybe Bool
    <*> arbitraryReducedMaybe n -- taskBaseLiked :: Maybe Bool
    <*> arbitraryReducedMaybe n -- taskBaseLikes :: Maybe [Like]
    <*> arbitraryReducedMaybe n -- taskBaseMemberships :: Maybe [TaskBaseAllOfMemberships]
    <*> arbitraryReducedMaybe n -- taskBaseModifiedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- taskBaseNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskBaseNumHearts :: Maybe Int
    <*> arbitraryReducedMaybe n -- taskBaseNumLikes :: Maybe Int
    <*> arbitraryReducedMaybe n -- taskBaseNumSubtasks :: Maybe Int
    <*> arbitraryReducedMaybe n -- taskBaseResourceSubtype :: Maybe E'ResourceSubtype2
    <*> arbitraryReducedMaybe n -- taskBaseStartOn :: Maybe Date
  
instance Arbitrary TaskBaseAllOf where
  arbitrary = sized genTaskBaseAllOf

genTaskBaseAllOf :: Int -> Gen TaskBaseAllOf
genTaskBaseAllOf n =
  TaskBaseAllOf
    <$> arbitraryReducedMaybe n -- taskBaseAllOfApprovalStatus :: Maybe E'ApprovalStatus
    <*> arbitraryReducedMaybe n -- taskBaseAllOfAssigneeStatus :: Maybe E'AssigneeStatus
    <*> arbitraryReducedMaybe n -- taskBaseAllOfCompleted :: Maybe Bool
    <*> arbitraryReducedMaybe n -- taskBaseAllOfCompletedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- taskBaseAllOfCompletedBy :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- taskBaseAllOfCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- taskBaseAllOfDependencies :: Maybe [AsanaResource]
    <*> arbitraryReducedMaybe n -- taskBaseAllOfDependents :: Maybe [AsanaResource]
    <*> arbitraryReducedMaybe n -- taskBaseAllOfDueAt :: Maybe Date
    <*> arbitraryReducedMaybe n -- taskBaseAllOfDueOn :: Maybe Date
    <*> arbitraryReducedMaybe n -- taskBaseAllOfExternal :: Maybe TaskBaseAllOfExternal
    <*> arbitraryReducedMaybe n -- taskBaseAllOfHtmlNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskBaseAllOfHearted :: Maybe Bool
    <*> arbitraryReducedMaybe n -- taskBaseAllOfHearts :: Maybe [Like]
    <*> arbitraryReducedMaybe n -- taskBaseAllOfIsRenderedAsSeparator :: Maybe Bool
    <*> arbitraryReducedMaybe n -- taskBaseAllOfLiked :: Maybe Bool
    <*> arbitraryReducedMaybe n -- taskBaseAllOfLikes :: Maybe [Like]
    <*> arbitraryReducedMaybe n -- taskBaseAllOfMemberships :: Maybe [TaskBaseAllOfMemberships]
    <*> arbitraryReducedMaybe n -- taskBaseAllOfModifiedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- taskBaseAllOfName :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskBaseAllOfNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskBaseAllOfNumHearts :: Maybe Int
    <*> arbitraryReducedMaybe n -- taskBaseAllOfNumLikes :: Maybe Int
    <*> arbitraryReducedMaybe n -- taskBaseAllOfNumSubtasks :: Maybe Int
    <*> arbitraryReducedMaybe n -- taskBaseAllOfResourceSubtype :: Maybe E'ResourceSubtype2
    <*> arbitraryReducedMaybe n -- taskBaseAllOfStartOn :: Maybe Date
  
instance Arbitrary TaskBaseAllOfExternal where
  arbitrary = sized genTaskBaseAllOfExternal

genTaskBaseAllOfExternal :: Int -> Gen TaskBaseAllOfExternal
genTaskBaseAllOfExternal n =
  TaskBaseAllOfExternal
    <$> arbitraryReducedMaybe n -- taskBaseAllOfExternalGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskBaseAllOfExternalData :: Maybe Text
  
instance Arbitrary TaskBaseAllOfMemberships where
  arbitrary = sized genTaskBaseAllOfMemberships

genTaskBaseAllOfMemberships :: Int -> Gen TaskBaseAllOfMemberships
genTaskBaseAllOfMemberships n =
  TaskBaseAllOfMemberships
    <$> arbitraryReducedMaybe n -- taskBaseAllOfMembershipsProject :: Maybe ProjectCompact
    <*> arbitraryReducedMaybe n -- taskBaseAllOfMembershipsSection :: Maybe SectionCompact
  
instance Arbitrary TaskCompact where
  arbitrary = sized genTaskCompact

genTaskCompact :: Int -> Gen TaskCompact
genTaskCompact n =
  TaskCompact
    <$> arbitraryReducedMaybe n -- taskCompactGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskCompactResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskCompactName :: Maybe Text
  
instance Arbitrary TaskCompactAllOf where
  arbitrary = sized genTaskCompactAllOf

genTaskCompactAllOf :: Int -> Gen TaskCompactAllOf
genTaskCompactAllOf n =
  TaskCompactAllOf
    <$> arbitraryReducedMaybe n -- taskCompactAllOfName :: Maybe Text
  
instance Arbitrary TaskCountResponse where
  arbitrary = sized genTaskCountResponse

genTaskCountResponse :: Int -> Gen TaskCountResponse
genTaskCountResponse n =
  TaskCountResponse
    <$> arbitraryReducedMaybe n -- taskCountResponseNumTasks :: Maybe Int
    <*> arbitraryReducedMaybe n -- taskCountResponseNumIncompleteTasks :: Maybe Int
    <*> arbitraryReducedMaybe n -- taskCountResponseNumCompletedTasks :: Maybe Int
    <*> arbitraryReducedMaybe n -- taskCountResponseNumMilestones :: Maybe Int
    <*> arbitraryReducedMaybe n -- taskCountResponseNumIncompleteMilestones :: Maybe Int
    <*> arbitraryReducedMaybe n -- taskCountResponseNumCompletedMilestones :: Maybe Int
  
instance Arbitrary TaskDuplicateRequest where
  arbitrary = sized genTaskDuplicateRequest

genTaskDuplicateRequest :: Int -> Gen TaskDuplicateRequest
genTaskDuplicateRequest n =
  TaskDuplicateRequest
    <$> arbitraryReducedMaybe n -- taskDuplicateRequestName :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskDuplicateRequestInclude :: Maybe E'Include2
  
instance Arbitrary TaskRemoveFollowersRequest where
  arbitrary = sized genTaskRemoveFollowersRequest

genTaskRemoveFollowersRequest :: Int -> Gen TaskRemoveFollowersRequest
genTaskRemoveFollowersRequest n =
  TaskRemoveFollowersRequest
    <$> arbitrary -- taskRemoveFollowersRequestFollowers :: [Text]
  
instance Arbitrary TaskRemoveProjectRequest where
  arbitrary = sized genTaskRemoveProjectRequest

genTaskRemoveProjectRequest :: Int -> Gen TaskRemoveProjectRequest
genTaskRemoveProjectRequest n =
  TaskRemoveProjectRequest
    <$> arbitrary -- taskRemoveProjectRequestProject :: Text
  
instance Arbitrary TaskRemoveTagRequest where
  arbitrary = sized genTaskRemoveTagRequest

genTaskRemoveTagRequest :: Int -> Gen TaskRemoveTagRequest
genTaskRemoveTagRequest n =
  TaskRemoveTagRequest
    <$> arbitrary -- taskRemoveTagRequestTag :: Text
  
instance Arbitrary TaskRequest where
  arbitrary = sized genTaskRequest

genTaskRequest :: Int -> Gen TaskRequest
genTaskRequest n =
  TaskRequest
    <$> arbitraryReducedMaybe n -- taskRequestGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskRequestResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskRequestName :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskRequestApprovalStatus :: Maybe E'ApprovalStatus
    <*> arbitraryReducedMaybe n -- taskRequestAssigneeStatus :: Maybe E'AssigneeStatus
    <*> arbitraryReducedMaybe n -- taskRequestCompleted :: Maybe Bool
    <*> arbitraryReducedMaybe n -- taskRequestCompletedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- taskRequestCompletedBy :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- taskRequestCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- taskRequestDependencies :: Maybe [AsanaResource]
    <*> arbitraryReducedMaybe n -- taskRequestDependents :: Maybe [AsanaResource]
    <*> arbitraryReducedMaybe n -- taskRequestDueAt :: Maybe Date
    <*> arbitraryReducedMaybe n -- taskRequestDueOn :: Maybe Date
    <*> arbitraryReducedMaybe n -- taskRequestExternal :: Maybe TaskBaseAllOfExternal
    <*> arbitraryReducedMaybe n -- taskRequestHtmlNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskRequestHearted :: Maybe Bool
    <*> arbitraryReducedMaybe n -- taskRequestHearts :: Maybe [Like]
    <*> arbitraryReducedMaybe n -- taskRequestIsRenderedAsSeparator :: Maybe Bool
    <*> arbitraryReducedMaybe n -- taskRequestLiked :: Maybe Bool
    <*> arbitraryReducedMaybe n -- taskRequestLikes :: Maybe [Like]
    <*> arbitraryReducedMaybe n -- taskRequestMemberships :: Maybe [TaskBaseAllOfMemberships]
    <*> arbitraryReducedMaybe n -- taskRequestModifiedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- taskRequestNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskRequestNumHearts :: Maybe Int
    <*> arbitraryReducedMaybe n -- taskRequestNumLikes :: Maybe Int
    <*> arbitraryReducedMaybe n -- taskRequestNumSubtasks :: Maybe Int
    <*> arbitraryReducedMaybe n -- taskRequestResourceSubtype :: Maybe E'ResourceSubtype2
    <*> arbitraryReducedMaybe n -- taskRequestStartOn :: Maybe Date
    <*> arbitraryReducedMaybe n -- taskRequestAssignee :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskRequestCustomFields :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- taskRequestFollowers :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- taskRequestParent :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskRequestProjects :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- taskRequestTags :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- taskRequestWorkspace :: Maybe Text
  
instance Arbitrary TaskRequestAllOf where
  arbitrary = sized genTaskRequestAllOf

genTaskRequestAllOf :: Int -> Gen TaskRequestAllOf
genTaskRequestAllOf n =
  TaskRequestAllOf
    <$> arbitraryReducedMaybe n -- taskRequestAllOfAssignee :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskRequestAllOfCustomFields :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- taskRequestAllOfFollowers :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- taskRequestAllOfParent :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskRequestAllOfProjects :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- taskRequestAllOfTags :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- taskRequestAllOfWorkspace :: Maybe Text
  
instance Arbitrary TaskResponse where
  arbitrary = sized genTaskResponse

genTaskResponse :: Int -> Gen TaskResponse
genTaskResponse n =
  TaskResponse
    <$> arbitraryReducedMaybe n -- taskResponseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskResponseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskResponseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskResponseApprovalStatus :: Maybe E'ApprovalStatus
    <*> arbitraryReducedMaybe n -- taskResponseAssigneeStatus :: Maybe E'AssigneeStatus
    <*> arbitraryReducedMaybe n -- taskResponseCompleted :: Maybe Bool
    <*> arbitraryReducedMaybe n -- taskResponseCompletedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- taskResponseCompletedBy :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- taskResponseCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- taskResponseDependencies :: Maybe [AsanaResource]
    <*> arbitraryReducedMaybe n -- taskResponseDependents :: Maybe [AsanaResource]
    <*> arbitraryReducedMaybe n -- taskResponseDueAt :: Maybe Date
    <*> arbitraryReducedMaybe n -- taskResponseDueOn :: Maybe Date
    <*> arbitraryReducedMaybe n -- taskResponseExternal :: Maybe TaskBaseAllOfExternal
    <*> arbitraryReducedMaybe n -- taskResponseHtmlNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskResponseHearted :: Maybe Bool
    <*> arbitraryReducedMaybe n -- taskResponseHearts :: Maybe [Like]
    <*> arbitraryReducedMaybe n -- taskResponseIsRenderedAsSeparator :: Maybe Bool
    <*> arbitraryReducedMaybe n -- taskResponseLiked :: Maybe Bool
    <*> arbitraryReducedMaybe n -- taskResponseLikes :: Maybe [Like]
    <*> arbitraryReducedMaybe n -- taskResponseMemberships :: Maybe [TaskBaseAllOfMemberships]
    <*> arbitraryReducedMaybe n -- taskResponseModifiedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- taskResponseNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskResponseNumHearts :: Maybe Int
    <*> arbitraryReducedMaybe n -- taskResponseNumLikes :: Maybe Int
    <*> arbitraryReducedMaybe n -- taskResponseNumSubtasks :: Maybe Int
    <*> arbitraryReducedMaybe n -- taskResponseResourceSubtype :: Maybe E'ResourceSubtype2
    <*> arbitraryReducedMaybe n -- taskResponseStartOn :: Maybe Date
    <*> arbitraryReducedMaybe n -- taskResponseAssignee :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- taskResponseCustomFields :: Maybe [CustomFieldResponse]
    <*> arbitraryReducedMaybe n -- taskResponseFollowers :: Maybe [UserCompact]
    <*> arbitraryReducedMaybe n -- taskResponseParent :: Maybe TaskCompact
    <*> arbitraryReducedMaybe n -- taskResponseProjects :: Maybe [ProjectCompact]
    <*> arbitraryReducedMaybe n -- taskResponseTags :: Maybe [TagCompact]
    <*> arbitraryReducedMaybe n -- taskResponseWorkspace :: Maybe WorkspaceCompact
  
instance Arbitrary TaskResponseAllOf where
  arbitrary = sized genTaskResponseAllOf

genTaskResponseAllOf :: Int -> Gen TaskResponseAllOf
genTaskResponseAllOf n =
  TaskResponseAllOf
    <$> arbitraryReducedMaybe n -- taskResponseAllOfAssignee :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- taskResponseAllOfCustomFields :: Maybe [CustomFieldResponse]
    <*> arbitraryReducedMaybe n -- taskResponseAllOfFollowers :: Maybe [UserCompact]
    <*> arbitraryReducedMaybe n -- taskResponseAllOfParent :: Maybe TaskCompact
    <*> arbitraryReducedMaybe n -- taskResponseAllOfProjects :: Maybe [ProjectCompact]
    <*> arbitraryReducedMaybe n -- taskResponseAllOfTags :: Maybe [TagCompact]
    <*> arbitraryReducedMaybe n -- taskResponseAllOfWorkspace :: Maybe WorkspaceCompact
  
instance Arbitrary TaskSetParentRequest where
  arbitrary = sized genTaskSetParentRequest

genTaskSetParentRequest :: Int -> Gen TaskSetParentRequest
genTaskSetParentRequest n =
  TaskSetParentRequest
    <$> arbitrary -- taskSetParentRequestParent :: Text
    <*> arbitraryReducedMaybe n -- taskSetParentRequestInsertAfter :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSetParentRequestInsertBefore :: Maybe Text
  
instance Arbitrary TeamAddUserRequest where
  arbitrary = sized genTeamAddUserRequest

genTeamAddUserRequest :: Int -> Gen TeamAddUserRequest
genTeamAddUserRequest n =
  TeamAddUserRequest
    <$> arbitraryReducedMaybe n -- teamAddUserRequestUser :: Maybe Text
  
instance Arbitrary TeamBase where
  arbitrary = sized genTeamBase

genTeamBase :: Int -> Gen TeamBase
genTeamBase n =
  
  pure TeamBase
   
instance Arbitrary TeamCompact where
  arbitrary = sized genTeamCompact

genTeamCompact :: Int -> Gen TeamCompact
genTeamCompact n =
  TeamCompact
    <$> arbitraryReducedMaybe n -- teamCompactGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- teamCompactResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- teamCompactName :: Maybe Text
  
instance Arbitrary TeamCompactAllOf where
  arbitrary = sized genTeamCompactAllOf

genTeamCompactAllOf :: Int -> Gen TeamCompactAllOf
genTeamCompactAllOf n =
  TeamCompactAllOf
    <$> arbitraryReducedMaybe n -- teamCompactAllOfName :: Maybe Text
  
instance Arbitrary TeamMembershipBase where
  arbitrary = sized genTeamMembershipBase

genTeamMembershipBase :: Int -> Gen TeamMembershipBase
genTeamMembershipBase n =
  
  pure TeamMembershipBase
   
instance Arbitrary TeamMembershipCompact where
  arbitrary = sized genTeamMembershipCompact

genTeamMembershipCompact :: Int -> Gen TeamMembershipCompact
genTeamMembershipCompact n =
  TeamMembershipCompact
    <$> arbitraryReducedMaybe n -- teamMembershipCompactGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- teamMembershipCompactResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- teamMembershipCompactUser :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- teamMembershipCompactTeam :: Maybe TeamCompact
    <*> arbitraryReducedMaybe n -- teamMembershipCompactIsGuest :: Maybe Bool
  
instance Arbitrary TeamMembershipCompactAllOf where
  arbitrary = sized genTeamMembershipCompactAllOf

genTeamMembershipCompactAllOf :: Int -> Gen TeamMembershipCompactAllOf
genTeamMembershipCompactAllOf n =
  TeamMembershipCompactAllOf
    <$> arbitraryReducedMaybe n -- teamMembershipCompactAllOfUser :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- teamMembershipCompactAllOfTeam :: Maybe TeamCompact
    <*> arbitraryReducedMaybe n -- teamMembershipCompactAllOfIsGuest :: Maybe Bool
  
instance Arbitrary TeamMembershipResponse where
  arbitrary = sized genTeamMembershipResponse

genTeamMembershipResponse :: Int -> Gen TeamMembershipResponse
genTeamMembershipResponse n =
  
  pure TeamMembershipResponse
   
instance Arbitrary TeamRemoveUserRequest where
  arbitrary = sized genTeamRemoveUserRequest

genTeamRemoveUserRequest :: Int -> Gen TeamRemoveUserRequest
genTeamRemoveUserRequest n =
  TeamRemoveUserRequest
    <$> arbitraryReducedMaybe n -- teamRemoveUserRequestUser :: Maybe Text
  
instance Arbitrary TeamRequest where
  arbitrary = sized genTeamRequest

genTeamRequest :: Int -> Gen TeamRequest
genTeamRequest n =
  
  pure TeamRequest
   
instance Arbitrary TeamResponse where
  arbitrary = sized genTeamResponse

genTeamResponse :: Int -> Gen TeamResponse
genTeamResponse n =
  TeamResponse
    <$> arbitraryReducedMaybe n -- teamResponseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- teamResponseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- teamResponseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- teamResponseDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- teamResponseHtmlDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- teamResponseOrganization :: Maybe WorkspaceCompact
  
instance Arbitrary TeamResponseAllOf where
  arbitrary = sized genTeamResponseAllOf

genTeamResponseAllOf :: Int -> Gen TeamResponseAllOf
genTeamResponseAllOf n =
  TeamResponseAllOf
    <$> arbitraryReducedMaybe n -- teamResponseAllOfDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- teamResponseAllOfHtmlDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- teamResponseAllOfOrganization :: Maybe WorkspaceCompact
  
instance Arbitrary UserBase where
  arbitrary = sized genUserBase

genUserBase :: Int -> Gen UserBase
genUserBase n =
  
  pure UserBase
   
instance Arbitrary UserCompact where
  arbitrary = sized genUserCompact

genUserCompact :: Int -> Gen UserCompact
genUserCompact n =
  UserCompact
    <$> arbitraryReducedMaybe n -- userCompactGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- userCompactResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- userCompactName :: Maybe Text
  
instance Arbitrary UserCompactAllOf where
  arbitrary = sized genUserCompactAllOf

genUserCompactAllOf :: Int -> Gen UserCompactAllOf
genUserCompactAllOf n =
  UserCompactAllOf
    <$> arbitraryReducedMaybe n -- userCompactAllOfName :: Maybe Text
  
instance Arbitrary UserRequest where
  arbitrary = sized genUserRequest

genUserRequest :: Int -> Gen UserRequest
genUserRequest n =
  
  pure UserRequest
   
instance Arbitrary UserResponse where
  arbitrary = sized genUserResponse

genUserResponse :: Int -> Gen UserResponse
genUserResponse n =
  UserResponse
    <$> arbitraryReducedMaybe n -- userResponseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- userResponseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- userResponseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userResponseEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- userResponsePhoto :: Maybe UserResponseAllOfPhoto
    <*> arbitraryReducedMaybe n -- userResponseWorkspaces :: Maybe [WorkspaceCompact]
  
instance Arbitrary UserResponseAllOf where
  arbitrary = sized genUserResponseAllOf

genUserResponseAllOf :: Int -> Gen UserResponseAllOf
genUserResponseAllOf n =
  UserResponseAllOf
    <$> arbitraryReducedMaybe n -- userResponseAllOfEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- userResponseAllOfPhoto :: Maybe UserResponseAllOfPhoto
    <*> arbitraryReducedMaybe n -- userResponseAllOfWorkspaces :: Maybe [WorkspaceCompact]
  
instance Arbitrary UserResponseAllOfPhoto where
  arbitrary = sized genUserResponseAllOfPhoto

genUserResponseAllOfPhoto :: Int -> Gen UserResponseAllOfPhoto
genUserResponseAllOfPhoto n =
  UserResponseAllOfPhoto
    <$> arbitraryReducedMaybe n -- userResponseAllOfPhotoImage21x21 :: Maybe Text
    <*> arbitraryReducedMaybe n -- userResponseAllOfPhotoImage27x27 :: Maybe Text
    <*> arbitraryReducedMaybe n -- userResponseAllOfPhotoImage36x36 :: Maybe Text
    <*> arbitraryReducedMaybe n -- userResponseAllOfPhotoImage60x60 :: Maybe Text
    <*> arbitraryReducedMaybe n -- userResponseAllOfPhotoImage128x128 :: Maybe Text
  
instance Arbitrary UserTaskListBase where
  arbitrary = sized genUserTaskListBase

genUserTaskListBase :: Int -> Gen UserTaskListBase
genUserTaskListBase n =
  
  pure UserTaskListBase
   
instance Arbitrary UserTaskListCompact where
  arbitrary = sized genUserTaskListCompact

genUserTaskListCompact :: Int -> Gen UserTaskListCompact
genUserTaskListCompact n =
  UserTaskListCompact
    <$> arbitraryReducedMaybe n -- userTaskListCompactGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- userTaskListCompactResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- userTaskListCompactName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userTaskListCompactOwner :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- userTaskListCompactWorkspace :: Maybe WorkspaceCompact
  
instance Arbitrary UserTaskListCompactAllOf where
  arbitrary = sized genUserTaskListCompactAllOf

genUserTaskListCompactAllOf :: Int -> Gen UserTaskListCompactAllOf
genUserTaskListCompactAllOf n =
  UserTaskListCompactAllOf
    <$> arbitraryReducedMaybe n -- userTaskListCompactAllOfName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userTaskListCompactAllOfOwner :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- userTaskListCompactAllOfWorkspace :: Maybe WorkspaceCompact
  
instance Arbitrary UserTaskListRequest where
  arbitrary = sized genUserTaskListRequest

genUserTaskListRequest :: Int -> Gen UserTaskListRequest
genUserTaskListRequest n =
  
  pure UserTaskListRequest
   
instance Arbitrary UserTaskListResponse where
  arbitrary = sized genUserTaskListResponse

genUserTaskListResponse :: Int -> Gen UserTaskListResponse
genUserTaskListResponse n =
  
  pure UserTaskListResponse
   
instance Arbitrary WebhookCompact where
  arbitrary = sized genWebhookCompact

genWebhookCompact :: Int -> Gen WebhookCompact
genWebhookCompact n =
  WebhookCompact
    <$> arbitraryReducedMaybe n -- webhookCompactGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- webhookCompactResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- webhookCompactActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- webhookCompactResource :: Maybe AsanaNamedResource
    <*> arbitraryReducedMaybe n -- webhookCompactTarget :: Maybe Text
  
instance Arbitrary WebhookCompactAllOf where
  arbitrary = sized genWebhookCompactAllOf

genWebhookCompactAllOf :: Int -> Gen WebhookCompactAllOf
genWebhookCompactAllOf n =
  WebhookCompactAllOf
    <$> arbitraryReducedMaybe n -- webhookCompactAllOfActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- webhookCompactAllOfResource :: Maybe AsanaNamedResource
    <*> arbitraryReducedMaybe n -- webhookCompactAllOfTarget :: Maybe Text
  
instance Arbitrary WebhookFilter where
  arbitrary = sized genWebhookFilter

genWebhookFilter :: Int -> Gen WebhookFilter
genWebhookFilter n =
  WebhookFilter
    <$> arbitraryReducedMaybe n -- webhookFilterResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- webhookFilterResourceSubtype :: Maybe Text
    <*> arbitraryReducedMaybe n -- webhookFilterAction :: Maybe Text
    <*> arbitraryReducedMaybe n -- webhookFilterFields :: Maybe [Text]
  
instance Arbitrary WebhookRequest where
  arbitrary = sized genWebhookRequest

genWebhookRequest :: Int -> Gen WebhookRequest
genWebhookRequest n =
  WebhookRequest
    <$> arbitrary -- webhookRequestResource :: Text
    <*> arbitrary -- webhookRequestTarget :: Text
    <*> arbitraryReducedMaybe n -- webhookRequestFilters :: Maybe [WebhookFilter]
  
instance Arbitrary WebhookResponse where
  arbitrary = sized genWebhookResponse

genWebhookResponse :: Int -> Gen WebhookResponse
genWebhookResponse n =
  WebhookResponse
    <$> arbitraryReducedMaybe n -- webhookResponseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- webhookResponseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- webhookResponseActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- webhookResponseResource :: Maybe AsanaNamedResource
    <*> arbitraryReducedMaybe n -- webhookResponseTarget :: Maybe Text
    <*> arbitraryReducedMaybe n -- webhookResponseCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- webhookResponseLastFailureAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- webhookResponseLastFailureContent :: Maybe Text
    <*> arbitraryReducedMaybe n -- webhookResponseLastSuccessAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- webhookResponseFilters :: Maybe [WebhookFilter]
  
instance Arbitrary WebhookResponseAllOf where
  arbitrary = sized genWebhookResponseAllOf

genWebhookResponseAllOf :: Int -> Gen WebhookResponseAllOf
genWebhookResponseAllOf n =
  WebhookResponseAllOf
    <$> arbitraryReducedMaybe n -- webhookResponseAllOfCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- webhookResponseAllOfLastFailureAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- webhookResponseAllOfLastFailureContent :: Maybe Text
    <*> arbitraryReducedMaybe n -- webhookResponseAllOfLastSuccessAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- webhookResponseAllOfFilters :: Maybe [WebhookFilter]
  
instance Arbitrary WorkspaceAddUserRequest where
  arbitrary = sized genWorkspaceAddUserRequest

genWorkspaceAddUserRequest :: Int -> Gen WorkspaceAddUserRequest
genWorkspaceAddUserRequest n =
  WorkspaceAddUserRequest
    <$> arbitraryReducedMaybe n -- workspaceAddUserRequestUser :: Maybe Text
  
instance Arbitrary WorkspaceBase where
  arbitrary = sized genWorkspaceBase

genWorkspaceBase :: Int -> Gen WorkspaceBase
genWorkspaceBase n =
  
  pure WorkspaceBase
   
instance Arbitrary WorkspaceCompact where
  arbitrary = sized genWorkspaceCompact

genWorkspaceCompact :: Int -> Gen WorkspaceCompact
genWorkspaceCompact n =
  WorkspaceCompact
    <$> arbitraryReducedMaybe n -- workspaceCompactGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- workspaceCompactResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- workspaceCompactName :: Maybe Text
  
instance Arbitrary WorkspaceCompactAllOf where
  arbitrary = sized genWorkspaceCompactAllOf

genWorkspaceCompactAllOf :: Int -> Gen WorkspaceCompactAllOf
genWorkspaceCompactAllOf n =
  WorkspaceCompactAllOf
    <$> arbitraryReducedMaybe n -- workspaceCompactAllOfName :: Maybe Text
  
instance Arbitrary WorkspaceMembershipBase where
  arbitrary = sized genWorkspaceMembershipBase

genWorkspaceMembershipBase :: Int -> Gen WorkspaceMembershipBase
genWorkspaceMembershipBase n =
  
  pure WorkspaceMembershipBase
   
instance Arbitrary WorkspaceMembershipCompact where
  arbitrary = sized genWorkspaceMembershipCompact

genWorkspaceMembershipCompact :: Int -> Gen WorkspaceMembershipCompact
genWorkspaceMembershipCompact n =
  WorkspaceMembershipCompact
    <$> arbitraryReducedMaybe n -- workspaceMembershipCompactGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- workspaceMembershipCompactResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- workspaceMembershipCompactUser :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- workspaceMembershipCompactWorkspace :: Maybe WorkspaceCompact
  
instance Arbitrary WorkspaceMembershipCompactAllOf where
  arbitrary = sized genWorkspaceMembershipCompactAllOf

genWorkspaceMembershipCompactAllOf :: Int -> Gen WorkspaceMembershipCompactAllOf
genWorkspaceMembershipCompactAllOf n =
  WorkspaceMembershipCompactAllOf
    <$> arbitraryReducedMaybe n -- workspaceMembershipCompactAllOfUser :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- workspaceMembershipCompactAllOfWorkspace :: Maybe WorkspaceCompact
  
instance Arbitrary WorkspaceMembershipRequest where
  arbitrary = sized genWorkspaceMembershipRequest

genWorkspaceMembershipRequest :: Int -> Gen WorkspaceMembershipRequest
genWorkspaceMembershipRequest n =
  
  pure WorkspaceMembershipRequest
   
instance Arbitrary WorkspaceMembershipResponse where
  arbitrary = sized genWorkspaceMembershipResponse

genWorkspaceMembershipResponse :: Int -> Gen WorkspaceMembershipResponse
genWorkspaceMembershipResponse n =
  WorkspaceMembershipResponse
    <$> arbitraryReducedMaybe n -- workspaceMembershipResponseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- workspaceMembershipResponseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- workspaceMembershipResponseUser :: Maybe UserCompact
    <*> arbitraryReducedMaybe n -- workspaceMembershipResponseWorkspace :: Maybe WorkspaceCompact
    <*> arbitraryReducedMaybe n -- workspaceMembershipResponseUserTaskList :: Maybe UserTaskListCompact
    <*> arbitraryReducedMaybe n -- workspaceMembershipResponseIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- workspaceMembershipResponseIsAdmin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- workspaceMembershipResponseIsGuest :: Maybe Bool
  
instance Arbitrary WorkspaceMembershipResponseAllOf where
  arbitrary = sized genWorkspaceMembershipResponseAllOf

genWorkspaceMembershipResponseAllOf :: Int -> Gen WorkspaceMembershipResponseAllOf
genWorkspaceMembershipResponseAllOf n =
  WorkspaceMembershipResponseAllOf
    <$> arbitraryReducedMaybe n -- workspaceMembershipResponseAllOfUserTaskList :: Maybe UserTaskListCompact
    <*> arbitraryReducedMaybe n -- workspaceMembershipResponseAllOfIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- workspaceMembershipResponseAllOfIsAdmin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- workspaceMembershipResponseAllOfIsGuest :: Maybe Bool
  
instance Arbitrary WorkspaceRemoveUserRequest where
  arbitrary = sized genWorkspaceRemoveUserRequest

genWorkspaceRemoveUserRequest :: Int -> Gen WorkspaceRemoveUserRequest
genWorkspaceRemoveUserRequest n =
  WorkspaceRemoveUserRequest
    <$> arbitraryReducedMaybe n -- workspaceRemoveUserRequestUser :: Maybe Text
  
instance Arbitrary WorkspaceRequest where
  arbitrary = sized genWorkspaceRequest

genWorkspaceRequest :: Int -> Gen WorkspaceRequest
genWorkspaceRequest n =
  
  pure WorkspaceRequest
   
instance Arbitrary WorkspaceResponse where
  arbitrary = sized genWorkspaceResponse

genWorkspaceResponse :: Int -> Gen WorkspaceResponse
genWorkspaceResponse n =
  WorkspaceResponse
    <$> arbitraryReducedMaybe n -- workspaceResponseGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- workspaceResponseResourceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- workspaceResponseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- workspaceResponseEmailDomains :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- workspaceResponseIsOrganization :: Maybe Bool
  
instance Arbitrary WorkspaceResponseAllOf where
  arbitrary = sized genWorkspaceResponseAllOf

genWorkspaceResponseAllOf :: Int -> Gen WorkspaceResponseAllOf
genWorkspaceResponseAllOf n =
  WorkspaceResponseAllOf
    <$> arbitraryReducedMaybe n -- workspaceResponseAllOfEmailDomains :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- workspaceResponseAllOfIsOrganization :: Maybe Bool
  



instance Arbitrary E'ApprovalStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'AssigneeStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'AssigneeStatus2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Color where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Color2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'CustomLabelPosition where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'DefaultView where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Format where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Include where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Include2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Method where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'ResourceSubtype where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'ResourceSubtype2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'ResourceSubtype3 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'ResourceType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'ResourceType2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'SortBy where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Source where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'State where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'WriteAccess where
  arbitrary = arbitraryBoundedEnum

