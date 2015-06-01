module Handler.Article.ArticleViewR (getArticleViewR) where

import Import

-- Show one article
getArticleViewR :: ArticleId -> Handler Html
getArticleViewR articleId = do
    -- The article
    article <- runDB $ get404 articleId

    -- The parent article (if any)
    let parentIdM = articleParent article
    parentM <- case parentIdM of
                    Just parentId -> runDB $ get parentId
                    Nothing -> return Nothing

    -- The child articles
    children <- runDB $ selectList [ArticleParent ==. Just articleId]
                                   [Asc ArticleWeight, Asc ArticleTitle]

    -- Author
    author <- runDB $ get (articleUser article)

    defaultLayout $ do
        setTitle $ toHtml (articleTitle article)
        setDescription $ articleDescription article
        setKeywords $ articleKeywords article
        setAuthor $ userUsername <$> author
        $(widgetFile "article/view")

