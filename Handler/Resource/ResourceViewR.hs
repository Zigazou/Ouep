module Handler.Resource.ResourceViewR (getResourceViewR, getResourceGetR) where

import Import

-- Display info about one resource
getResourceViewR :: ResourceId -> Handler Html
getResourceViewR resourceId = do
    -- The resource
    resource <- runDB $ get404 resourceId
    userIdM <- maybeAuthId

    let visible = isJust userIdM || resourcePublic resource

    defaultLayout $ do
        $(widgetFile "resource/view")

-- Send one resource
getResourceGetR :: ResourceId -> Handler ()
getResourceGetR resourceId = do
    -- The resource
    resource <- runDB $ get404 resourceId

    sendFile (resourceMimetype resource)
             (resourceDirectory </> unpack (resourceFilename resource))

