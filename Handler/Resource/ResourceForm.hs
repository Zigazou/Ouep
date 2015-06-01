module Handler.Resource.ResourceForm (resnewForm, resmodForm) where

import Import

resnewForm :: Form (Text, FileInfo, Text, Int, Bool)
resnewForm = renderDivs $ (,,,,)
    <$> areq textField "Identifiant" Nothing
    <*> areq fileField "Fichier"     Nothing
    <*> areq textField "Description" Nothing
    <*> areq intField  "Poids"       (Just 0)
    <*> areq boolField "Public ?"    (Just True)

resmodForm :: Resource -> Form (Text, Maybe FileInfo, Text, Int, Bool)
resmodForm resource = renderDivs $ (,,,,)
    <$> areq textField "Identifiant" (Just $ resourceResname     resource)
    <*> aopt fileField "Fichier"     Nothing
    <*> areq textField "Description" (Just $ resourceDescription resource)
    <*> areq intField  "Poids"       (Just $ resourceWeight      resource)
    <*> areq boolField "Public ?"    (Just $ resourcePublic      resource)

