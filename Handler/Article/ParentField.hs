module Handler.Article.ParentField (parentField) where

import Import

-- Handles field parsing
parentFieldParse :: YesodPersist site
                 => YesodPersistBackend site ~ SqlBackend
                 => RenderMessage (HandlerSite (HandlerT site IO)) FormMessage
                 => [Text]
                 -> [FileInfo]
                 -> HandlerT site IO
                        (Either (SomeMessage (HandlerSite (HandlerT site IO)))
                                (Maybe (Key Article))
                        )
parentFieldParse rawVals _ = do
    let (name:[]) = rawVals
    case name of
        "" -> return $ Right Nothing
        _ -> do
            articleM <- runDB $ getBy (UniqueArt name)
            return $ case articleM of
                Nothing      -> (Left . SomeMessage)
                                ("Identifiant d’article invalide" :: Text)
                Just article -> (Right . Just) (entityKey article)

-- Handles field viewing
parentFieldView :: YesodPersist site
                => YesodPersistBackend site ~ SqlBackend
                => FieldViewFunc (HandlerT site IO) ArticleId
parentFieldView ident name attrs resultE isReq =
    case resultE of
        Left err -> parentHtml ident name attrs (Left err) isReq
        Right key -> do
            articleM <- handlerToWidget . runDB $ get key
            let value = case articleM of
                           Just article -> Right (articleArtname article)
                           Nothing      -> Left ("Clé d’article invalide." :: Text)

            parentHtml ident name attrs value isReq

    where parentHtml identifier fieldName attributes value isRequired =
            [whamlet|$newline never
                <input id="#{identifier}"
                       name="#{fieldName}"
                       *{attributes}
                       type="text"
                       :isRequired:required
                       value="#{either id id value}">
            |]

parentField :: YesodPersist site
            => YesodPersistBackend site ~ SqlBackend
            => RenderMessage (HandlerSite (HandlerT site IO)) FormMessage
            => Field (HandlerT site IO) ArticleId
parentField = Field
    { fieldParse   = parentFieldParse
    , fieldView    = parentFieldView
    , fieldEnctype = UrlEncoded
    }

