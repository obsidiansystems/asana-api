{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import Asana.Model
import Asana.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy AddCustomFieldSettingRequest)
      propMimeEq MimeJSON (Proxy :: Proxy AddFollowersRequest)
      propMimeEq MimeJSON (Proxy :: Proxy AddMembersRequest)
      propMimeEq MimeJSON (Proxy :: Proxy AsanaNamedResource)
      propMimeEq MimeJSON (Proxy :: Proxy AsanaNamedResourceAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy AsanaResource)
      propMimeEq MimeJSON (Proxy :: Proxy AttachmentBase)
      propMimeEq MimeJSON (Proxy :: Proxy AttachmentCompact)
      propMimeEq MimeJSON (Proxy :: Proxy AttachmentCompactAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy AttachmentRequest)
      propMimeEq MimeJSON (Proxy :: Proxy AttachmentResponse)
      propMimeEq MimeJSON (Proxy :: Proxy AttachmentResponseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy BatchRequest)
      propMimeEq MimeJSON (Proxy :: Proxy BatchRequestAction)
      propMimeEq MimeJSON (Proxy :: Proxy BatchRequestActionOptions)
      propMimeEq MimeJSON (Proxy :: Proxy BatchResponse)
      propMimeEq MimeJSON (Proxy :: Proxy CustomFieldBase)
      propMimeEq MimeJSON (Proxy :: Proxy CustomFieldBaseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy CustomFieldCompact)
      propMimeEq MimeJSON (Proxy :: Proxy CustomFieldCompactAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy CustomFieldRequest)
      propMimeEq MimeJSON (Proxy :: Proxy CustomFieldRequestAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy CustomFieldResponse)
      propMimeEq MimeJSON (Proxy :: Proxy CustomFieldResponseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy CustomFieldSettingBase)
      propMimeEq MimeJSON (Proxy :: Proxy CustomFieldSettingCompact)
      propMimeEq MimeJSON (Proxy :: Proxy CustomFieldSettingResponse)
      propMimeEq MimeJSON (Proxy :: Proxy CustomFieldSettingResponseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy EnumOption)
      propMimeEq MimeJSON (Proxy :: Proxy EnumOptionAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy EnumOptionBase)
      propMimeEq MimeJSON (Proxy :: Proxy EnumOptionInsertRequest)
      propMimeEq MimeJSON (Proxy :: Proxy EnumOptionRequest)
      propMimeEq MimeJSON (Proxy :: Proxy EnumOptionRequestAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy Error)
      propMimeEq MimeJSON (Proxy :: Proxy ErrorResponse)
      propMimeEq MimeJSON (Proxy :: Proxy EventResponse)
      propMimeEq MimeJSON (Proxy :: Proxy EventResponseChange)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject1)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject10)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject11)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject12)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject13)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject14)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject15)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject16)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject17)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject18)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject19)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject2)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject20)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject21)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject22)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject23)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject24)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject25)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject26)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject27)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject28)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject29)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject3)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject30)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject31)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject32)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject33)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject34)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject35)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject36)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject37)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject38)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject39)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject4)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject40)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject41)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject42)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject43)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject44)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject45)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject46)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject47)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject48)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject49)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject5)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject50)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject51)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject52)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject53)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject54)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject55)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject6)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject7)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject8)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject9)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse200)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2001)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20010)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20011)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20012)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20013)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20014)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20015)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20016)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20017)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20018)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20019)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2002)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20020)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20021)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20022)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20023)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20024)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20025)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20026)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20027)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20028)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20029)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2003)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20030)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20031)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20032)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20033)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20034)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20035)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20036)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse20037)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2004)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2005)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2006)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2007)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2008)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2009)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse201)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2011)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2012)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2013)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2014)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2015)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2016)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2017)
      propMimeEq MimeJSON (Proxy :: Proxy JobBase)
      propMimeEq MimeJSON (Proxy :: Proxy JobCompact)
      propMimeEq MimeJSON (Proxy :: Proxy JobCompactAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy JobResponse)
      propMimeEq MimeJSON (Proxy :: Proxy Like)
      propMimeEq MimeJSON (Proxy :: Proxy ModifyDependenciesRequest)
      propMimeEq MimeJSON (Proxy :: Proxy ModifyDependentsRequest)
      propMimeEq MimeJSON (Proxy :: Proxy OrganizationExportBase)
      propMimeEq MimeJSON (Proxy :: Proxy OrganizationExportCompact)
      propMimeEq MimeJSON (Proxy :: Proxy OrganizationExportCompactAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy OrganizationExportRequest)
      propMimeEq MimeJSON (Proxy :: Proxy OrganizationExportResponse)
      propMimeEq MimeJSON (Proxy :: Proxy PortfolioAddItemRequest)
      propMimeEq MimeJSON (Proxy :: Proxy PortfolioBase)
      propMimeEq MimeJSON (Proxy :: Proxy PortfolioBaseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy PortfolioCompact)
      propMimeEq MimeJSON (Proxy :: Proxy PortfolioCompactAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy PortfolioMembershipBase)
      propMimeEq MimeJSON (Proxy :: Proxy PortfolioMembershipCompact)
      propMimeEq MimeJSON (Proxy :: Proxy PortfolioMembershipCompactAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy PortfolioMembershipResponse)
      propMimeEq MimeJSON (Proxy :: Proxy PortfolioRemoveItemRequest)
      propMimeEq MimeJSON (Proxy :: Proxy PortfolioRequest)
      propMimeEq MimeJSON (Proxy :: Proxy PortfolioRequestAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy PortfolioResponse)
      propMimeEq MimeJSON (Proxy :: Proxy PortfolioResponseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy Preview)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectBase)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectBaseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectCompact)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectCompactAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectDuplicateRequest)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectDuplicateRequestScheduleDates)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectMembershipBase)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectMembershipCompact)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectMembershipCompactAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectMembershipResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectMembershipResponseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectRequest)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectRequestAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectResponseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectSectionInsertRequest)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectStatusBase)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectStatusBaseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectStatusCompact)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectStatusCompactAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectStatusRequest)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectStatusResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectStatusResponseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy RemoveCustomFieldSettingRequest)
      propMimeEq MimeJSON (Proxy :: Proxy RemoveFollowersRequest)
      propMimeEq MimeJSON (Proxy :: Proxy RemoveMembersRequest)
      propMimeEq MimeJSON (Proxy :: Proxy SectionBase)
      propMimeEq MimeJSON (Proxy :: Proxy SectionCompact)
      propMimeEq MimeJSON (Proxy :: Proxy SectionCompactAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy SectionRequest)
      propMimeEq MimeJSON (Proxy :: Proxy SectionResponse)
      propMimeEq MimeJSON (Proxy :: Proxy SectionResponseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy SectionTaskInsertRequest)
      propMimeEq MimeJSON (Proxy :: Proxy StoryBase)
      propMimeEq MimeJSON (Proxy :: Proxy StoryBaseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy StoryCompact)
      propMimeEq MimeJSON (Proxy :: Proxy StoryCompactAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy StoryRequest)
      propMimeEq MimeJSON (Proxy :: Proxy StoryResponse)
      propMimeEq MimeJSON (Proxy :: Proxy StoryResponseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy StoryResponseDates)
      propMimeEq MimeJSON (Proxy :: Proxy TagBase)
      propMimeEq MimeJSON (Proxy :: Proxy TagBaseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy TagCompact)
      propMimeEq MimeJSON (Proxy :: Proxy TagCompactAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy TagRequest)
      propMimeEq MimeJSON (Proxy :: Proxy TagRequestAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy TagResponse)
      propMimeEq MimeJSON (Proxy :: Proxy TagResponseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy TaskAddFollowersRequest)
      propMimeEq MimeJSON (Proxy :: Proxy TaskAddProjectRequest)
      propMimeEq MimeJSON (Proxy :: Proxy TaskAddTagRequest)
      propMimeEq MimeJSON (Proxy :: Proxy TaskBase)
      propMimeEq MimeJSON (Proxy :: Proxy TaskBaseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy TaskBaseAllOfExternal)
      propMimeEq MimeJSON (Proxy :: Proxy TaskBaseAllOfMemberships)
      propMimeEq MimeJSON (Proxy :: Proxy TaskCompact)
      propMimeEq MimeJSON (Proxy :: Proxy TaskCompactAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy TaskCountResponse)
      propMimeEq MimeJSON (Proxy :: Proxy TaskDuplicateRequest)
      propMimeEq MimeJSON (Proxy :: Proxy TaskRemoveFollowersRequest)
      propMimeEq MimeJSON (Proxy :: Proxy TaskRemoveProjectRequest)
      propMimeEq MimeJSON (Proxy :: Proxy TaskRemoveTagRequest)
      propMimeEq MimeJSON (Proxy :: Proxy TaskRequest)
      propMimeEq MimeJSON (Proxy :: Proxy TaskRequestAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy TaskResponse)
      propMimeEq MimeJSON (Proxy :: Proxy TaskResponseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy TaskSetParentRequest)
      propMimeEq MimeJSON (Proxy :: Proxy TeamAddUserRequest)
      propMimeEq MimeJSON (Proxy :: Proxy TeamBase)
      propMimeEq MimeJSON (Proxy :: Proxy TeamCompact)
      propMimeEq MimeJSON (Proxy :: Proxy TeamCompactAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy TeamMembershipBase)
      propMimeEq MimeJSON (Proxy :: Proxy TeamMembershipCompact)
      propMimeEq MimeJSON (Proxy :: Proxy TeamMembershipCompactAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy TeamMembershipResponse)
      propMimeEq MimeJSON (Proxy :: Proxy TeamRemoveUserRequest)
      propMimeEq MimeJSON (Proxy :: Proxy TeamRequest)
      propMimeEq MimeJSON (Proxy :: Proxy TeamResponse)
      propMimeEq MimeJSON (Proxy :: Proxy TeamResponseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy UserBase)
      propMimeEq MimeJSON (Proxy :: Proxy UserCompact)
      propMimeEq MimeJSON (Proxy :: Proxy UserCompactAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy UserRequest)
      propMimeEq MimeJSON (Proxy :: Proxy UserResponse)
      propMimeEq MimeJSON (Proxy :: Proxy UserResponseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy UserResponseAllOfPhoto)
      propMimeEq MimeJSON (Proxy :: Proxy UserTaskListBase)
      propMimeEq MimeJSON (Proxy :: Proxy UserTaskListCompact)
      propMimeEq MimeJSON (Proxy :: Proxy UserTaskListCompactAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy UserTaskListRequest)
      propMimeEq MimeJSON (Proxy :: Proxy UserTaskListResponse)
      propMimeEq MimeJSON (Proxy :: Proxy WebhookCompact)
      propMimeEq MimeJSON (Proxy :: Proxy WebhookCompactAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy WebhookFilter)
      propMimeEq MimeJSON (Proxy :: Proxy WebhookRequest)
      propMimeEq MimeJSON (Proxy :: Proxy WebhookResponse)
      propMimeEq MimeJSON (Proxy :: Proxy WebhookResponseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy WorkspaceAddUserRequest)
      propMimeEq MimeJSON (Proxy :: Proxy WorkspaceBase)
      propMimeEq MimeJSON (Proxy :: Proxy WorkspaceCompact)
      propMimeEq MimeJSON (Proxy :: Proxy WorkspaceCompactAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy WorkspaceMembershipBase)
      propMimeEq MimeJSON (Proxy :: Proxy WorkspaceMembershipCompact)
      propMimeEq MimeJSON (Proxy :: Proxy WorkspaceMembershipCompactAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy WorkspaceMembershipRequest)
      propMimeEq MimeJSON (Proxy :: Proxy WorkspaceMembershipResponse)
      propMimeEq MimeJSON (Proxy :: Proxy WorkspaceMembershipResponseAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy WorkspaceRemoveUserRequest)
      propMimeEq MimeJSON (Proxy :: Proxy WorkspaceRequest)
      propMimeEq MimeJSON (Proxy :: Proxy WorkspaceResponse)
      propMimeEq MimeJSON (Proxy :: Proxy WorkspaceResponseAllOf)
      
