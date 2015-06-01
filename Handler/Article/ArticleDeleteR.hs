module Handler.Article.ArticleDeleteR (getArticleDeleteR, postArticleDeleteR) where

import Import

-- Delete an article
postArticleDeleteR :: ArticleId -> Handler Html
postArticleDeleteR articleId = do
    _ <- requireAuthId
    article <- runDB $ get404 articleId
    runDB $ delete articleId
    setMessage [shamlet|Article #{articleTitle article} supprimé|]
    redirectUltDest HomeR

getArticleDeleteR :: ArticleId -> Handler Html
getArticleDeleteR articleId = do
    _ <- requireAuthId
    article <- runDB $ get404 articleId
    let title = [shamlet|Supprimer l’article #{articleTitle article} ?|]
    defaultLayout $ do
        setTitle title
        $(widgetFile "article/delete")


