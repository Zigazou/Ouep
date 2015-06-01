module Handler.Resource.ResourceDeleteR (getResourceDeleteR, postResourceDeleteR) where

import Import
import Handler.Resource.FileManagement

-- Delete a resource
postResourceDeleteR :: ResourceId -> Handler Html
postResourceDeleteR resourceId = do
    _ <- requireAuthId
    resource <- runDB $ get404 resourceId
    runDB $ delete resourceId
    deleteFile $ resourceDirectory </> (resourceFilename resource)
    setMessage . toHtml $ ("Ressource supprimée" :: Text)
    redirectUltDest HomeR

getResourceDeleteR :: ResourceId -> Handler Html
getResourceDeleteR resourceId = do
    _ <- requireAuthId
    resource <- runDB $ get404 resourceId
    let title = [shamlet|Supprimer la ressource #{resourceResname resource} ?|]
    defaultLayout $ do
        setTitle title
        $(widgetFile "resource/delete")

