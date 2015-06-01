module Handler.Resource.ResourceNewR (getResourceNewR, postResourceNewR) where

import Import
import Handler.Resource.FileManagement
import Handler.Resource.ResourceForm

-- Create an resource
getResourceNewR :: Handler Html
getResourceNewR = do
    _ <- requireAuthId
    (formWidget, enctype) <- generateFormPost resnewForm
    defaultLayout $ do
        setTitle "Ajouter une nouvelle ressource"
        $(widgetFile "resource/new")

postResourceNewR :: Handler Html
postResourceNewR = do
    userId <- requireAuthId
    lastUpdate <- liftIO getCurrentTime
    ((formResult, _), _) <- runFormPost $ resnewForm
    
    case formResult of
        FormSuccess (resname, file, desc, weight, public) -> do
            let resource = Resource
                    { resourceResname     = resname
                    , resourceFilename    = unpack (fileName file)
                    , resourceMimetype    = encodeUtf8 (fileContentType file)
                    , resourceDescription = desc
                    , resourceUser        = userId
                    , resourceLastUpdate  = lastUpdate
                    , resourceWeight      = weight
                    , resourcePublic      = public
                    }
            _ <- writeToServer file
            resourceId <- runDB $ insert resource
            setMessage "Nouvelle ressource créée"
            redirectUltDest $ ResourceViewR resourceId
        FormMissing -> do
            setMessage "Aucune donnée disponible"
            redirectUltDest ResourceNewR
        FormFailure ts -> do
            setMessage "Erreur dans le formulaire"
            mapM_ (setMessage . toHtml) ts
            redirectUltDest ResourceNewR

