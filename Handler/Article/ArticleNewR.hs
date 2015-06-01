module Handler.Article.ArticleNewR (getArticleNewR, postArticleNewR) where

import Import
import Handler.Article.ArticleForm

-- Create an article
getArticleNewR :: Handler Html
getArticleNewR = do
    userId <- requireAuthId
    (formWidget, enctype) <- generateFormPost $ articleForm userId Nothing
    defaultLayout $ do
        setTitle "Ajouter un nouvel article"
        $(widgetFile "article/new")

postArticleNewR :: Handler Html
postArticleNewR = do
    userId <- requireAuthId
    ((formResult, _), _) <- runFormPost $ articleForm userId Nothing

    case formResult of
        FormSuccess article -> do
            articleId <- runDB $ insert article
            setMessage "Nouvel article créé"
            redirectUltDest $ ArticleViewR articleId
        FormMissing -> do
            setMessage "Aucune donnée disponible"
            redirectUltDest ArticleNewR
        FormFailure ts -> do
            setMessage "Erreur dans le formulaire"
            mapM_ (setMessage . toHtml) ts
            redirectUltDest ArticleNewR

