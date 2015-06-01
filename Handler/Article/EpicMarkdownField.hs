module Handler.Article.EpicMarkdownField (epicMarkdownField) where

import Import
import Yesod.Markdown (Markdown(Markdown), unMarkdown)
import qualified Data.Aeson.Types as A

-- | Parse a Markdown field (convert Text to Markdown)
epicFieldParse :: (RenderMessage site FormMessage, Monad m)
               => [Text] -> [FileInfo]
               -> m (Either (SomeMessage site) (Maybe Markdown))
epicFieldParse = parseHelper $ Right . Markdown . filter (/= '\r')

-- | Display a Markdown field (convert Markdown to Widget
epicFieldView :: (MonadWidget site, HandlerSite site ~ App)
              => Text -> Text -> [(Text, Text)] -> Either Text Markdown -> Bool
              -> site ()
epicFieldView identifier fieldName attributes value _required = do
    let container = identifier ++ ("-epic" :: Text)
    addScript $ StaticR library_epiceditor_js_epiceditor_min_js
    toWidget [hamlet|$newline never
        <div id=#{container}>
            <textarea id=#{identifier} name=#{fieldName} *{attributes}>
                #{either id unMarkdown value}
    |]

    toWidget [julius|
        var options = {
          container: #{A.String container},
          textarea: document.getElementById(#{A.String identifier}),
          autogrow: { minHeight: 200, maxHeight:600},
          theme: {
            base   : "@{StaticR library_epiceditor_themes_base_epiceditor_css}"
          , preview: "@{StaticR css_epic_preview_css}"
          , editor : "@{StaticR library_epiceditor_themes_editor_epic_dark_css}"
          }
        };
        new EpicEditor(options).load();
    |]

-- | An epicMarkdownField embeds the Epic Editor to give a better user
--   experience.
epicMarkdownField :: Monad site
                  => HandlerSite site ~ App
                  => RenderMessage (HandlerSite site) FormMessage
                  => Field site Markdown
epicMarkdownField = Field epicFieldParse epicFieldView UrlEncoded

