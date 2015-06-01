module Handler.Resource.ResourceEditR (getResourceEditR, postResourceEditR) where

import Import
import Handler.Resource.ResourceForm

-- Edit a resource
getResourceEditR :: ResourceId -> Handler Html
getResourceEditR resourceId = do
    _ <- requireAuthId
    resource <- runDB $ get404 resourceId
    (formWidget, enctype) <- generateFormPost $ resmodForm resource
    defaultLayout $ do
        setTitle $ toHtml (resourceResname resource)
        $(widgetFile "resource/edit")

postResourceEditR :: ResourceId -> Handler Html
postResourceEditR resourceId = do
    userId <- requireAuthId
    current <- runDB $ get404 resourceId
    lastUpdate <- liftIO getCurrentTime
    ((formResult, _), _) <- runFormPost $ resmodForm current

    case formResult of
        FormSuccess (resname, fileM, desc, weight, public) -> do
            let (newFilename, newMimetype) = case fileM of
                    Nothing   -> ( resourceFilename current
                                 , resourceMimetype current
                                 )
                    Just file -> ( unpack (fileName file)
                                 , encodeUtf8 (fileContentType file)
                                 )
                resource = Resource { resourceResname     = resname
                                    , resourceFilename    = newFilename
                                    , resourceMimetype    = newMimetype
                                    , resourceDescription = desc
                                    , resourceUser        = userId
                                    , resourceLastUpdate  = lastUpdate
                                    , resourceWeight      = weight
                                    , resourcePublic      = public
                                    }
            _ <- runDB $ replace resourceId resource
            setMessage "Ressource mise à jour"
        FormMissing -> do
            setMessage "Aucune donnée disponible"
            redirectUltDest $ ResourceEditR resourceId
        FormFailure ts -> do
            setMessage "Erreur dans le formulaire"
            mapM_ (setMessage . toHtml) ts
            redirectUltDest $ ResourceEditR resourceId

    redirectUltDest $ ResourceViewR resourceId

