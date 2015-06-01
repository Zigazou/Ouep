module Handler.Article.ArticleEditR (getArticleEditR, postArticleEditR) where

import Import
import Handler.Article.ArticleForm

-- Edit an article
getArticleEditR :: ArticleId -> Handler Html
getArticleEditR articleId = do
    userId <- requireAuthId
    article <- runDB $ get404 articleId
    (formWidget, enctype) <- generateFormPost $ articleForm userId (Just article)
    defaultLayout $ do
        setTitle $ toHtml (articleTitle article)
        $(widgetFile "article/edit")

postArticleEditR :: ArticleId -> Handler Html
postArticleEditR articleId = do
    userId <- requireAuthId
    ((formResult, _), _) <- runFormPost $ articleForm userId Nothing
    case formResult of
        FormSuccess article -> do
            _ <- runDB $ replace articleId article
            setMessage "Article mis à jour"
        _ -> setMessage "Erreur dans le formulaire"

    redirectUltDest $ ArticleViewR articleId

