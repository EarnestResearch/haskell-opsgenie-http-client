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

import OpsgenieREST.Model
import OpsgenieREST.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy AlertActionPayload)
      propMimeEq MimeJSON (Proxy :: Proxy AlertAttachment)
      propMimeEq MimeJSON (Proxy :: Proxy AlertAttachmentMeta)
      propMimeEq MimeJSON (Proxy :: Proxy AlertIntegration)
      propMimeEq MimeJSON (Proxy :: Proxy AlertLog)
      propMimeEq MimeJSON (Proxy :: Proxy AlertNote)
      propMimeEq MimeJSON (Proxy :: Proxy AlertPaging)
      propMimeEq MimeJSON (Proxy :: Proxy AlertRecipient)
      propMimeEq MimeJSON (Proxy :: Proxy AlertReport)
      propMimeEq MimeJSON (Proxy :: Proxy AlertRequestStatus)
      propMimeEq MimeJSON (Proxy :: Proxy AlertTeamMeta)
      propMimeEq MimeJSON (Proxy :: Proxy AlertUserMeta)
      propMimeEq MimeJSON (Proxy :: Proxy BaseAlert)
      propMimeEq MimeJSON (Proxy :: Proxy BaseResponse)
      propMimeEq MimeJSON (Proxy :: Proxy Condition)
      propMimeEq MimeJSON (Proxy :: Proxy CreateSavedSearchPayload)
      propMimeEq MimeJSON (Proxy :: Proxy Duration)
      propMimeEq MimeJSON (Proxy :: Proxy Filter)
      propMimeEq MimeJSON (Proxy :: Proxy GetCountAlertsResponseData)
      propMimeEq MimeJSON (Proxy :: Proxy PageDetails)
      propMimeEq MimeJSON (Proxy :: Proxy Recipient)
      propMimeEq MimeJSON (Proxy :: Proxy Responder)
      propMimeEq MimeJSON (Proxy :: Proxy SavedSearch)
      propMimeEq MimeJSON (Proxy :: Proxy SavedSearchEntity)
      propMimeEq MimeJSON (Proxy :: Proxy SavedSearchMeta)
      propMimeEq MimeJSON (Proxy :: Proxy SuccessData)
      propMimeEq MimeJSON (Proxy :: Proxy TimeOfDayRestriction)
      propMimeEq MimeJSON (Proxy :: Proxy TimeRestrictionInterval)
      propMimeEq MimeJSON (Proxy :: Proxy UpdateAlertDescriptionPayload)
      propMimeEq MimeJSON (Proxy :: Proxy UpdateAlertMessagePayload)
      propMimeEq MimeJSON (Proxy :: Proxy UpdateAlertPriorityPayload)
      propMimeEq MimeJSON (Proxy :: Proxy UpdateSavedSearchPayload)
      propMimeEq MimeJSON (Proxy :: Proxy WeekdayTimeRestriction)
      propMimeEq MimeJSON (Proxy :: Proxy AcknowledgeAlertPayload)
      propMimeEq MimeJSON (Proxy :: Proxy AddDetailsToAlertPayload)
      propMimeEq MimeJSON (Proxy :: Proxy AddNoteToAlertPayload)
      propMimeEq MimeJSON (Proxy :: Proxy AddResponderToAlertPayload)
      propMimeEq MimeJSON (Proxy :: Proxy AddTagsToAlertPayload)
      propMimeEq MimeJSON (Proxy :: Proxy AddTeamToAlertPayload)
      propMimeEq MimeJSON (Proxy :: Proxy Alert)
      propMimeEq MimeJSON (Proxy :: Proxy AllRecipient)
      propMimeEq MimeJSON (Proxy :: Proxy AssignAlertPayload)
      propMimeEq MimeJSON (Proxy :: Proxy BaseResponseWithExpandable)
      propMimeEq MimeJSON (Proxy :: Proxy CloseAlertPayload)
      propMimeEq MimeJSON (Proxy :: Proxy CreateAlertPayload)
      propMimeEq MimeJSON (Proxy :: Proxy CreateSavedSearchResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ErrorResponse)
      propMimeEq MimeJSON (Proxy :: Proxy EscalateAlertToNextPayload)
      propMimeEq MimeJSON (Proxy :: Proxy EscalationRecipient)
      propMimeEq MimeJSON (Proxy :: Proxy ExecuteCustomAlertActionPayload)
      propMimeEq MimeJSON (Proxy :: Proxy GetAlertAttachmentResponse)
      propMimeEq MimeJSON (Proxy :: Proxy GetAlertResponse)
      propMimeEq MimeJSON (Proxy :: Proxy GetCountAlertsResponse)
      propMimeEq MimeJSON (Proxy :: Proxy GetRequestStatusResponse)
      propMimeEq MimeJSON (Proxy :: Proxy GetSavedSearchResponse)
      propMimeEq MimeJSON (Proxy :: Proxy GroupRecipient)
      propMimeEq MimeJSON (Proxy :: Proxy ListAlertAttachmentsResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ListAlertLogsResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ListAlertNotesResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ListAlertRecipientsResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ListAlertsResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ListSavedSearchesResponse)
      propMimeEq MimeJSON (Proxy :: Proxy MatchAll)
      propMimeEq MimeJSON (Proxy :: Proxy MatchAllConditions)
      propMimeEq MimeJSON (Proxy :: Proxy MatchAnyCondition)
      propMimeEq MimeJSON (Proxy :: Proxy NoRecipient)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleRecipient)
      propMimeEq MimeJSON (Proxy :: Proxy SnoozeAlertPayload)
      propMimeEq MimeJSON (Proxy :: Proxy SuccessResponse)
      propMimeEq MimeJSON (Proxy :: Proxy TeamRecipient)
      propMimeEq MimeJSON (Proxy :: Proxy TeamResponder)
      propMimeEq MimeJSON (Proxy :: Proxy TimeOfDayRestrictionInterval)
      propMimeEq MimeJSON (Proxy :: Proxy UnAcknowledgeAlertPayload)
      propMimeEq MimeJSON (Proxy :: Proxy UserRecipient)
      propMimeEq MimeJSON (Proxy :: Proxy UserResponder)
      propMimeEq MimeJSON (Proxy :: Proxy WeekdayTimeRestrictionInterval)
      
