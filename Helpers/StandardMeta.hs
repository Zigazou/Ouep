module Helpers.StandardMeta where

import Foundation
import Import.NoFoundation

setAuthor :: Maybe Text -> Widget
setAuthor authorM = toWidgetHead
    [hamlet|
        $maybe author <- authorM
            <meta name=author content="#{author}">
    |]

setKeywords :: Maybe Text -> Widget
setKeywords keywordsM = toWidgetHead
    [hamlet|
        $maybe keywords <- keywordsM
            <meta name=keywords content="#{keywords}">
    |]

setDescription :: Maybe Text -> Widget
setDescription descriptionM = toWidgetHead
    [hamlet|
        $maybe description <- descriptionM
            <meta name=description content="#{description}">
    |]

