module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    articles <- runDB $ selectList [ArticleParent ==. Nothing]
                                   [Asc ArticleWeight, Asc ArticleTitle]

    defaultLayout $ do
        setTitle "Ouep.eu, le serveur de Zigazou"
        $(widgetFile "homepage")

