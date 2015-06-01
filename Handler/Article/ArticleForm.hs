module Handler.Article.ArticleForm (articleForm) where

import Import
--import Yesod.Markdown (markdownField)
import Handler.Article.EpicMarkdownField (epicMarkdownField)
import Handler.Article.ParentField

defaultValue :: Maybe a -> a -> Maybe a
defaultValue Nothing value = Just value
defaultValue value _       = value

-- Article form for Create, Update, Delete
articleForm :: UserId -> Maybe Article -> Form Article
articleForm userId articleM = renderDivs $ Article
    <$> areq textField "Identifiant"
             (articleArtname <$> articleM)
    <*> areq textField "Titre"
             (articleTitle <$> articleM)
    <*> aopt textField "Mots-clés"
             (articleKeywords <$> articleM)
    <*> aopt textField "Description"
             (articleDescription <$> articleM)
    <*> areq epicMarkdownField "Corps"
             (articleBody <$> articleM)
    <*> aopt parentField "Parent"
             (articleParent <$> articleM)
    <*> pure userId
    <*> lift (liftIO getCurrentTime)
    <*> areq intField "Poids"
             (defaultValue (articleWeight <$> articleM) 0)
    <*> areq boolField "Public ?"
             (defaultValue (articlePublic <$> articleM) True)

