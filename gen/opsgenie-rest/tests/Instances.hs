{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Instances where

import OpsgenieREST.Model
import OpsgenieREST.Core

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

-- * Models
 
instance Arbitrary AlertActionPayload where
  arbitrary =
    AlertActionPayload
      <$> arbitrary -- alertActionPayloadUser :: Maybe Text
      <*> arbitrary -- alertActionPayloadNote :: Maybe Text
      <*> arbitrary -- alertActionPayloadSource :: Maybe Text
    
instance Arbitrary AlertAttachment where
  arbitrary =
    AlertAttachment
      <$> arbitrary -- alertAttachmentName :: Maybe Text
      <*> arbitrary -- alertAttachmentUrl :: Maybe Text
    
instance Arbitrary AlertAttachmentMeta where
  arbitrary =
    AlertAttachmentMeta
      <$> arbitrary -- alertAttachmentMetaName :: Maybe Text
      <*> arbitrary -- alertAttachmentMetaId :: Maybe Text
    
instance Arbitrary AlertIntegration where
  arbitrary =
    AlertIntegration
      <$> arbitrary -- alertIntegrationId :: Maybe Text
      <*> arbitrary -- alertIntegrationName :: Maybe Text
      <*> arbitrary -- alertIntegrationType :: Maybe Text
    
instance Arbitrary AlertLog where
  arbitrary =
    AlertLog
      <$> arbitrary -- alertLogLog :: Maybe Text
      <*> arbitrary -- alertLogType :: Maybe Text
      <*> arbitrary -- alertLogOwner :: Maybe Text
      <*> arbitrary -- alertLogCreatedAt :: Maybe DateTime
      <*> arbitrary -- alertLogOffset :: Maybe Text
    
instance Arbitrary AlertNote where
  arbitrary =
    AlertNote
      <$> arbitrary -- alertNoteNote :: Maybe Text
      <*> arbitrary -- alertNoteOwner :: Maybe Text
      <*> arbitrary -- alertNoteCreatedAt :: Maybe DateTime
      <*> arbitrary -- alertNoteOffset :: Maybe Text
    
instance Arbitrary AlertPaging where
  arbitrary =
    AlertPaging
      <$> arbitrary -- alertPagingFirst :: Maybe Text
      <*> arbitrary -- alertPagingNext :: Maybe Text
    
instance Arbitrary AlertRecipient where
  arbitrary =
    AlertRecipient
      <$> arbitrary -- alertRecipientUser :: Maybe AlertUserMeta
      <*> arbitrary -- alertRecipientState :: Maybe Text
      <*> arbitrary -- alertRecipientMethod :: Maybe Text
      <*> arbitrary -- alertRecipientCreatedAt :: Maybe DateTime
      <*> arbitrary -- alertRecipientUpdatedAt :: Maybe DateTime
    
instance Arbitrary AlertReport where
  arbitrary =
    AlertReport
      <$> arbitrary -- alertReportAckTime :: Maybe Integer
      <*> arbitrary -- alertReportCloseTime :: Maybe Integer
      <*> arbitrary -- alertReportAcknowledgedBy :: Maybe Text
      <*> arbitrary -- alertReportClosedBy :: Maybe Text
    
instance Arbitrary AlertRequestStatus where
  arbitrary =
    AlertRequestStatus
      <$> arbitrary -- alertRequestStatusAction :: Maybe Text
      <*> arbitrary -- alertRequestStatusProcessedAt :: Maybe DateTime
      <*> arbitrary -- alertRequestStatusIntegrationId :: Maybe Text
      <*> arbitrary -- alertRequestStatusIsSuccess :: Maybe Bool
      <*> arbitrary -- alertRequestStatusStatus :: Maybe Text
      <*> arbitrary -- alertRequestStatusAlertId :: Maybe Text
      <*> arbitrary -- alertRequestStatusAlias :: Maybe Text
    
instance Arbitrary AlertTeamMeta where
  arbitrary =
    AlertTeamMeta
      <$> arbitrary -- alertTeamMetaId :: Text
    
instance Arbitrary AlertUserMeta where
  arbitrary =
    AlertUserMeta
      <$> arbitrary -- alertUserMetaId :: Maybe Text
      <*> arbitrary -- alertUserMetaUsername :: Maybe Text
    
instance Arbitrary BaseAlert where
  arbitrary =
    BaseAlert
      <$> arbitrary -- baseAlertId :: Text
      <*> arbitrary -- baseAlertTinyId :: Maybe Text
      <*> arbitrary -- baseAlertAlias :: Maybe Text
      <*> arbitrary -- baseAlertMessage :: Maybe Text
      <*> arbitrary -- baseAlertStatus :: Maybe Text
      <*> arbitrary -- baseAlertAcknowledged :: Maybe Bool
      <*> arbitrary -- baseAlertIsSeen :: Maybe Bool
      <*> arbitrary -- baseAlertTags :: Maybe [Text]
      <*> arbitrary -- baseAlertSnoozed :: Maybe Bool
      <*> arbitrary -- baseAlertSnoozedUntil :: Maybe DateTime
      <*> arbitrary -- baseAlertCount :: Maybe Int
      <*> arbitrary -- baseAlertLastOccurredAt :: Maybe DateTime
      <*> arbitrary -- baseAlertCreatedAt :: Maybe DateTime
      <*> arbitrary -- baseAlertUpdatedAt :: Maybe DateTime
      <*> arbitrary -- baseAlertSource :: Maybe Text
      <*> arbitrary -- baseAlertOwner :: Maybe Text
      <*> arbitrary -- baseAlertPriority :: Maybe Text
      <*> arbitrary -- baseAlertResponders :: Maybe [Responder]
      <*> arbitrary -- baseAlertIntegration :: Maybe AlertIntegration
      <*> arbitrary -- baseAlertReport :: Maybe AlertReport
    
instance Arbitrary BaseResponse where
  arbitrary =
    BaseResponse
      <$> arbitrary -- baseResponseRequestId :: Text
      <*> arbitrary -- baseResponseTook :: Float
    
instance Arbitrary Condition where
  arbitrary =
    Condition
      <$> arbitrary -- conditionField :: Text
      <*> arbitrary -- conditionKey :: Maybe Text
      <*> arbitrary -- conditionNot :: Maybe Bool
      <*> arbitrary -- conditionOperation :: Text
      <*> arbitrary -- conditionExpectedValue :: Maybe Text
      <*> arbitrary -- conditionOrder :: Maybe Integer
    
instance Arbitrary CreateSavedSearchPayload where
  arbitrary =
    CreateSavedSearchPayload
      <$> arbitrary -- createSavedSearchPayloadName :: Text
      <*> arbitrary -- createSavedSearchPayloadDescription :: Maybe Text
      <*> arbitrary -- createSavedSearchPayloadQuery :: Text
      <*> arbitrary -- createSavedSearchPayloadOwner :: UserRecipient
      <*> arbitrary -- createSavedSearchPayloadTeams :: Maybe [TeamRecipient]
    
instance Arbitrary Duration where
  arbitrary =
    Duration
      <$> arbitrary -- durationTimeAmount :: Integer
      <*> arbitrary -- durationTimeUnit :: Maybe Text
    
instance Arbitrary Filter where
  arbitrary =
    Filter
      <$> arbitrary -- filterType :: Text
    
instance Arbitrary GetCountAlertsResponseData where
  arbitrary =
    GetCountAlertsResponseData
      <$> arbitrary -- getCountAlertsResponseDataCount :: Maybe Int
    
instance Arbitrary PageDetails where
  arbitrary =
    PageDetails
      <$> arbitrary -- pageDetailsPrev :: Maybe Text
      <*> arbitrary -- pageDetailsNext :: Maybe Text
      <*> arbitrary -- pageDetailsFirst :: Maybe Text
      <*> arbitrary -- pageDetailsLast :: Maybe Text
    
instance Arbitrary Recipient where
  arbitrary =
    Recipient
      <$> arbitrary -- recipientType :: Text
      <*> arbitrary -- recipientId :: Maybe Text
    
instance Arbitrary Responder where
  arbitrary =
    Responder
      <$> arbitrary -- responderType :: Text
      <*> arbitrary -- responderId :: Text
    
instance Arbitrary SavedSearch where
  arbitrary =
    SavedSearch
      <$> arbitrary -- savedSearchId :: Maybe Text
      <*> arbitrary -- savedSearchName :: Maybe Text
      <*> arbitrary -- savedSearchCreatedAt :: Maybe DateTime
      <*> arbitrary -- savedSearchUpdatedAt :: Maybe DateTime
      <*> arbitrary -- savedSearchOwner :: Maybe SavedSearchEntity
      <*> arbitrary -- savedSearchTeams :: Maybe [SavedSearchEntity]
      <*> arbitrary -- savedSearchDescription :: Maybe Text
      <*> arbitrary -- savedSearchQuery :: Maybe Text
    
instance Arbitrary SavedSearchEntity where
  arbitrary =
    SavedSearchEntity
      <$> arbitrary -- savedSearchEntityId :: Maybe Text
    
instance Arbitrary SavedSearchMeta where
  arbitrary =
    SavedSearchMeta
      <$> arbitrary -- savedSearchMetaId :: Maybe Text
      <*> arbitrary -- savedSearchMetaName :: Maybe Text
    
instance Arbitrary SuccessData where
  arbitrary =
    SuccessData
      <$> arbitrary -- successDataId :: Maybe Text
      <*> arbitrary -- successDataName :: Maybe Text
    
instance Arbitrary TimeOfDayRestriction where
  arbitrary =
    TimeOfDayRestriction
      <$> arbitrary -- timeOfDayRestrictionStartHour :: Maybe Int
      <*> arbitrary -- timeOfDayRestrictionStartMin :: Maybe Int
      <*> arbitrary -- timeOfDayRestrictionEndHour :: Maybe Int
      <*> arbitrary -- timeOfDayRestrictionEndMin :: Maybe Int
    
instance Arbitrary TimeRestrictionInterval where
  arbitrary =
    TimeRestrictionInterval
      <$> arbitrary -- timeRestrictionIntervalType :: Text
    
instance Arbitrary UpdateAlertDescriptionPayload where
  arbitrary =
    UpdateAlertDescriptionPayload
      <$> arbitrary -- updateAlertDescriptionPayloadDescription :: Text
    
instance Arbitrary UpdateAlertMessagePayload where
  arbitrary =
    UpdateAlertMessagePayload
      <$> arbitrary -- updateAlertMessagePayloadMessage :: Text
    
instance Arbitrary UpdateAlertPriorityPayload where
  arbitrary =
    UpdateAlertPriorityPayload
      <$> arbitrary -- updateAlertPriorityPayloadPriority :: Text
    
instance Arbitrary UpdateSavedSearchPayload where
  arbitrary =
    UpdateSavedSearchPayload
      <$> arbitrary -- updateSavedSearchPayloadName :: Text
      <*> arbitrary -- updateSavedSearchPayloadDescription :: Maybe Text
      <*> arbitrary -- updateSavedSearchPayloadQuery :: Text
      <*> arbitrary -- updateSavedSearchPayloadOwner :: UserRecipient
      <*> arbitrary -- updateSavedSearchPayloadTeams :: Maybe [TeamRecipient]
    
instance Arbitrary WeekdayTimeRestriction where
  arbitrary =
    WeekdayTimeRestriction
      <$> arbitrary -- weekdayTimeRestrictionStartDay :: Maybe Text
      <*> arbitrary -- weekdayTimeRestrictionStartHour :: Maybe Int
      <*> arbitrary -- weekdayTimeRestrictionStartMin :: Maybe Int
      <*> arbitrary -- weekdayTimeRestrictionEndDay :: Maybe Text
      <*> arbitrary -- weekdayTimeRestrictionEndHour :: Maybe Int
      <*> arbitrary -- weekdayTimeRestrictionEndMin :: Maybe Int
    
instance Arbitrary AcknowledgeAlertPayload where
  arbitrary =
    AcknowledgeAlertPayload
      <$> arbitrary -- acknowledgeAlertPayloadUser :: Maybe Text
      <*> arbitrary -- acknowledgeAlertPayloadNote :: Maybe Text
      <*> arbitrary -- acknowledgeAlertPayloadSource :: Maybe Text
    
instance Arbitrary AddDetailsToAlertPayload where
  arbitrary =
    AddDetailsToAlertPayload
      <$> arbitrary -- addDetailsToAlertPayloadUser :: Maybe Text
      <*> arbitrary -- addDetailsToAlertPayloadNote :: Maybe Text
      <*> arbitrary -- addDetailsToAlertPayloadSource :: Maybe Text
      <*> arbitrary -- addDetailsToAlertPayloadDetails :: (Map.Map String Text)
    
instance Arbitrary AddNoteToAlertPayload where
  arbitrary =
    AddNoteToAlertPayload
      <$> arbitrary -- addNoteToAlertPayloadUser :: Maybe Text
      <*> arbitrary -- addNoteToAlertPayloadNote :: Maybe Text
      <*> arbitrary -- addNoteToAlertPayloadSource :: Maybe Text
    
instance Arbitrary AddResponderToAlertPayload where
  arbitrary =
    AddResponderToAlertPayload
      <$> arbitrary -- addResponderToAlertPayloadUser :: Maybe Text
      <*> arbitrary -- addResponderToAlertPayloadNote :: Maybe Text
      <*> arbitrary -- addResponderToAlertPayloadSource :: Maybe Text
      <*> arbitrary -- addResponderToAlertPayloadResponder :: Recipient
    
instance Arbitrary AddTagsToAlertPayload where
  arbitrary =
    AddTagsToAlertPayload
      <$> arbitrary -- addTagsToAlertPayloadUser :: Maybe Text
      <*> arbitrary -- addTagsToAlertPayloadNote :: Maybe Text
      <*> arbitrary -- addTagsToAlertPayloadSource :: Maybe Text
      <*> arbitrary -- addTagsToAlertPayloadTags :: [Text]
    
instance Arbitrary AddTeamToAlertPayload where
  arbitrary =
    AddTeamToAlertPayload
      <$> arbitrary -- addTeamToAlertPayloadUser :: Maybe Text
      <*> arbitrary -- addTeamToAlertPayloadNote :: Maybe Text
      <*> arbitrary -- addTeamToAlertPayloadSource :: Maybe Text
      <*> arbitrary -- addTeamToAlertPayloadTeam :: TeamRecipient
    
instance Arbitrary Alert where
  arbitrary =
    Alert
      <$> arbitrary -- alertId :: Text
      <*> arbitrary -- alertTinyId :: Maybe Text
      <*> arbitrary -- alertAlias :: Maybe Text
      <*> arbitrary -- alertMessage :: Maybe Text
      <*> arbitrary -- alertStatus :: Maybe Text
      <*> arbitrary -- alertAcknowledged :: Maybe Bool
      <*> arbitrary -- alertIsSeen :: Maybe Bool
      <*> arbitrary -- alertTags :: Maybe [Text]
      <*> arbitrary -- alertSnoozed :: Maybe Bool
      <*> arbitrary -- alertSnoozedUntil :: Maybe DateTime
      <*> arbitrary -- alertCount :: Maybe Int
      <*> arbitrary -- alertLastOccurredAt :: Maybe DateTime
      <*> arbitrary -- alertCreatedAt :: Maybe DateTime
      <*> arbitrary -- alertUpdatedAt :: Maybe DateTime
      <*> arbitrary -- alertSource :: Maybe Text
      <*> arbitrary -- alertOwner :: Maybe Text
      <*> arbitrary -- alertPriority :: Maybe Text
      <*> arbitrary -- alertResponders :: Maybe [Responder]
      <*> arbitrary -- alertIntegration :: Maybe AlertIntegration
      <*> arbitrary -- alertReport :: Maybe AlertReport
      <*> arbitrary -- alertActions :: Maybe [Text]
      <*> arbitrary -- alertEntity :: Maybe Text
      <*> arbitrary -- alertDescription :: Maybe Text
      <*> arbitrary -- alertDetails :: Maybe (Map.Map String Text)
    
instance Arbitrary AllRecipient where
  arbitrary =
    AllRecipient
      <$> arbitrary -- allRecipientType :: Text
      <*> arbitrary -- allRecipientId :: Maybe Text
    
instance Arbitrary AssignAlertPayload where
  arbitrary =
    AssignAlertPayload
      <$> arbitrary -- assignAlertPayloadUser :: Maybe Text
      <*> arbitrary -- assignAlertPayloadNote :: Maybe Text
      <*> arbitrary -- assignAlertPayloadSource :: Maybe Text
      <*> arbitrary -- assignAlertPayloadOwner :: UserRecipient
    
instance Arbitrary BaseResponseWithExpandable where
  arbitrary =
    BaseResponseWithExpandable
      <$> arbitrary -- baseResponseWithExpandableRequestId :: Text
      <*> arbitrary -- baseResponseWithExpandableTook :: Float
      <*> arbitrary -- baseResponseWithExpandableExpandable :: Maybe [Text]
    
instance Arbitrary CloseAlertPayload where
  arbitrary =
    CloseAlertPayload
      <$> arbitrary -- closeAlertPayloadUser :: Maybe Text
      <*> arbitrary -- closeAlertPayloadNote :: Maybe Text
      <*> arbitrary -- closeAlertPayloadSource :: Maybe Text
    
instance Arbitrary CreateAlertPayload where
  arbitrary =
    CreateAlertPayload
      <$> arbitrary -- createAlertPayloadUser :: Maybe Text
      <*> arbitrary -- createAlertPayloadNote :: Maybe Text
      <*> arbitrary -- createAlertPayloadSource :: Maybe Text
      <*> arbitrary -- createAlertPayloadMessage :: Text
      <*> arbitrary -- createAlertPayloadAlias :: Maybe Text
      <*> arbitrary -- createAlertPayloadDescription :: Maybe Text
      <*> arbitrary -- createAlertPayloadResponders :: Maybe [Recipient]
      <*> arbitrary -- createAlertPayloadVisibleTo :: Maybe [Recipient]
      <*> arbitrary -- createAlertPayloadActions :: Maybe [Text]
      <*> arbitrary -- createAlertPayloadTags :: Maybe [Text]
      <*> arbitrary -- createAlertPayloadDetails :: Maybe (Map.Map String Text)
      <*> arbitrary -- createAlertPayloadEntity :: Maybe Text
      <*> arbitrary -- createAlertPayloadPriority :: Maybe Text
    
instance Arbitrary CreateSavedSearchResponse where
  arbitrary =
    CreateSavedSearchResponse
      <$> arbitrary -- createSavedSearchResponseRequestId :: Text
      <*> arbitrary -- createSavedSearchResponseTook :: Float
      <*> arbitrary -- createSavedSearchResponseData :: Maybe SavedSearchMeta
    
instance Arbitrary ErrorResponse where
  arbitrary =
    ErrorResponse
      <$> arbitrary -- errorResponseRequestId :: Text
      <*> arbitrary -- errorResponseTook :: Float
      <*> arbitrary -- errorResponseMessage :: Maybe Text
      <*> arbitrary -- errorResponseCode :: Maybe Int
      <*> arbitrary -- errorResponseResponseHeaders :: Maybe (Map.Map String [Text])
    
instance Arbitrary EscalateAlertToNextPayload where
  arbitrary =
    EscalateAlertToNextPayload
      <$> arbitrary -- escalateAlertToNextPayloadUser :: Maybe Text
      <*> arbitrary -- escalateAlertToNextPayloadNote :: Maybe Text
      <*> arbitrary -- escalateAlertToNextPayloadSource :: Maybe Text
      <*> arbitrary -- escalateAlertToNextPayloadEscalation :: EscalationRecipient
    
instance Arbitrary EscalationRecipient where
  arbitrary =
    EscalationRecipient
      <$> arbitrary -- escalationRecipientType :: Text
      <*> arbitrary -- escalationRecipientId :: Maybe Text
      <*> arbitrary -- escalationRecipientName :: Maybe Text
    
instance Arbitrary ExecuteCustomAlertActionPayload where
  arbitrary =
    ExecuteCustomAlertActionPayload
      <$> arbitrary -- executeCustomAlertActionPayloadUser :: Maybe Text
      <*> arbitrary -- executeCustomAlertActionPayloadNote :: Maybe Text
      <*> arbitrary -- executeCustomAlertActionPayloadSource :: Maybe Text
    
instance Arbitrary GetAlertAttachmentResponse where
  arbitrary =
    GetAlertAttachmentResponse
      <$> arbitrary -- getAlertAttachmentResponseRequestId :: Text
      <*> arbitrary -- getAlertAttachmentResponseTook :: Float
      <*> arbitrary -- getAlertAttachmentResponseData :: Maybe AlertAttachment
    
instance Arbitrary GetAlertResponse where
  arbitrary =
    GetAlertResponse
      <$> arbitrary -- getAlertResponseRequestId :: Text
      <*> arbitrary -- getAlertResponseTook :: Float
      <*> arbitrary -- getAlertResponseData :: Maybe Alert
    
instance Arbitrary GetCountAlertsResponse where
  arbitrary =
    GetCountAlertsResponse
      <$> arbitrary -- getCountAlertsResponseRequestId :: Text
      <*> arbitrary -- getCountAlertsResponseTook :: Float
      <*> arbitrary -- getCountAlertsResponseData :: Maybe GetCountAlertsResponseData
    
instance Arbitrary GetRequestStatusResponse where
  arbitrary =
    GetRequestStatusResponse
      <$> arbitrary -- getRequestStatusResponseRequestId :: Text
      <*> arbitrary -- getRequestStatusResponseTook :: Float
      <*> arbitrary -- getRequestStatusResponseData :: Maybe AlertRequestStatus
    
instance Arbitrary GetSavedSearchResponse where
  arbitrary =
    GetSavedSearchResponse
      <$> arbitrary -- getSavedSearchResponseRequestId :: Text
      <*> arbitrary -- getSavedSearchResponseTook :: Float
      <*> arbitrary -- getSavedSearchResponseData :: Maybe SavedSearch
    
instance Arbitrary GroupRecipient where
  arbitrary =
    GroupRecipient
      <$> arbitrary -- groupRecipientType :: Text
      <*> arbitrary -- groupRecipientId :: Maybe Text
      <*> arbitrary -- groupRecipientName :: Maybe Text
    
instance Arbitrary ListAlertAttachmentsResponse where
  arbitrary =
    ListAlertAttachmentsResponse
      <$> arbitrary -- listAlertAttachmentsResponseRequestId :: Text
      <*> arbitrary -- listAlertAttachmentsResponseTook :: Float
      <*> arbitrary -- listAlertAttachmentsResponseData :: Maybe [AlertAttachmentMeta]
    
instance Arbitrary ListAlertLogsResponse where
  arbitrary =
    ListAlertLogsResponse
      <$> arbitrary -- listAlertLogsResponseRequestId :: Text
      <*> arbitrary -- listAlertLogsResponseTook :: Float
      <*> arbitrary -- listAlertLogsResponseData :: Maybe [AlertLog]
      <*> arbitrary -- listAlertLogsResponsePaging :: Maybe AlertPaging
    
instance Arbitrary ListAlertNotesResponse where
  arbitrary =
    ListAlertNotesResponse
      <$> arbitrary -- listAlertNotesResponseRequestId :: Text
      <*> arbitrary -- listAlertNotesResponseTook :: Float
      <*> arbitrary -- listAlertNotesResponseData :: Maybe [AlertNote]
      <*> arbitrary -- listAlertNotesResponsePaging :: Maybe AlertPaging
    
instance Arbitrary ListAlertRecipientsResponse where
  arbitrary =
    ListAlertRecipientsResponse
      <$> arbitrary -- listAlertRecipientsResponseRequestId :: Text
      <*> arbitrary -- listAlertRecipientsResponseTook :: Float
      <*> arbitrary -- listAlertRecipientsResponseData :: Maybe [AlertRecipient]
    
instance Arbitrary ListAlertsResponse where
  arbitrary =
    ListAlertsResponse
      <$> arbitrary -- listAlertsResponseRequestId :: Text
      <*> arbitrary -- listAlertsResponseTook :: Float
      <*> arbitrary -- listAlertsResponseData :: Maybe [BaseAlert]
      <*> arbitrary -- listAlertsResponsePaging :: Maybe PageDetails
    
instance Arbitrary ListSavedSearchesResponse where
  arbitrary =
    ListSavedSearchesResponse
      <$> arbitrary -- listSavedSearchesResponseRequestId :: Text
      <*> arbitrary -- listSavedSearchesResponseTook :: Float
      <*> arbitrary -- listSavedSearchesResponseData :: Maybe [SavedSearchMeta]
    
instance Arbitrary MatchAll where
  arbitrary =
    MatchAll
      <$> arbitrary -- matchAllType :: Text
    
instance Arbitrary MatchAllConditions where
  arbitrary =
    MatchAllConditions
      <$> arbitrary -- matchAllConditionsType :: Text
      <*> arbitrary -- matchAllConditionsConditions :: Maybe [Condition]
    
instance Arbitrary MatchAnyCondition where
  arbitrary =
    MatchAnyCondition
      <$> arbitrary -- matchAnyConditionType :: Text
      <*> arbitrary -- matchAnyConditionConditions :: Maybe [Condition]
    
instance Arbitrary NoRecipient where
  arbitrary =
    NoRecipient
      <$> arbitrary -- noRecipientType :: Text
      <*> arbitrary -- noRecipientId :: Maybe Text
    
instance Arbitrary ScheduleRecipient where
  arbitrary =
    ScheduleRecipient
      <$> arbitrary -- scheduleRecipientType :: Text
      <*> arbitrary -- scheduleRecipientId :: Maybe Text
      <*> arbitrary -- scheduleRecipientName :: Maybe Text
    
instance Arbitrary SnoozeAlertPayload where
  arbitrary =
    SnoozeAlertPayload
      <$> arbitrary -- snoozeAlertPayloadUser :: Maybe Text
      <*> arbitrary -- snoozeAlertPayloadNote :: Maybe Text
      <*> arbitrary -- snoozeAlertPayloadSource :: Maybe Text
      <*> arbitrary -- snoozeAlertPayloadEndTime :: DateTime
    
instance Arbitrary SuccessResponse where
  arbitrary =
    SuccessResponse
      <$> arbitrary -- successResponseRequestId :: Text
      <*> arbitrary -- successResponseTook :: Float
      <*> arbitrary -- successResponseResult :: Maybe Text
      <*> arbitrary -- successResponseData :: Maybe SuccessData
    
instance Arbitrary TeamRecipient where
  arbitrary =
    TeamRecipient
      <$> arbitrary -- teamRecipientType :: Text
      <*> arbitrary -- teamRecipientId :: Maybe Text
      <*> arbitrary -- teamRecipientName :: Maybe Text
    
instance Arbitrary TeamResponder where
  arbitrary =
    TeamResponder
      <$> arbitrary -- teamResponderType :: Text
      <*> arbitrary -- teamResponderId :: Text
      <*> arbitrary -- teamResponderName :: Maybe Text
    
instance Arbitrary TimeOfDayRestrictionInterval where
  arbitrary =
    TimeOfDayRestrictionInterval
      <$> arbitrary -- timeOfDayRestrictionIntervalType :: Text
      <*> arbitrary -- timeOfDayRestrictionIntervalRestriction :: Maybe TimeOfDayRestriction
    
instance Arbitrary UnAcknowledgeAlertPayload where
  arbitrary =
    UnAcknowledgeAlertPayload
      <$> arbitrary -- unAcknowledgeAlertPayloadUser :: Maybe Text
      <*> arbitrary -- unAcknowledgeAlertPayloadNote :: Maybe Text
      <*> arbitrary -- unAcknowledgeAlertPayloadSource :: Maybe Text
    
instance Arbitrary UserRecipient where
  arbitrary =
    UserRecipient
      <$> arbitrary -- userRecipientType :: Text
      <*> arbitrary -- userRecipientId :: Maybe Text
      <*> arbitrary -- userRecipientUsername :: Maybe Text
    
instance Arbitrary UserResponder where
  arbitrary =
    UserResponder
      <$> arbitrary -- userResponderType :: Text
      <*> arbitrary -- userResponderId :: Text
    
instance Arbitrary WeekdayTimeRestrictionInterval where
  arbitrary =
    WeekdayTimeRestrictionInterval
      <$> arbitrary -- weekdayTimeRestrictionIntervalType :: Text
      <*> arbitrary -- weekdayTimeRestrictionIntervalRestrictions :: Maybe [WeekdayTimeRestriction]
    



instance Arbitrary E'Direction where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Field where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'IdentifierType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Operation where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Order where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Priority where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'SearchIdentifierType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Sort where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'StartDay where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'TimeUnit where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type3 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type4 where
  arbitrary = arbitraryBoundedEnum
