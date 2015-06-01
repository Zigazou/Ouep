module Handler.Resource.ResourceListR (getResourceListR) where

import Import
import Data.Text.Read

data Constraint = Constraint
        { constraintMin     :: Int
        , constraintMax     :: Int
        , constraintDefault :: Int
        }

constraintApply :: Constraint -> Int -> Int
constraintApply cst value
    | value < constraintMin cst = constraintDefault cst
    | value > constraintMax cst = constraintDefault cst
    | otherwise                 = value

lookupGetParamInt :: MonadHandler m => Text -> Constraint -> m Int
lookupGetParamInt paramName cst = do
    valueM <- lookupGetParam paramName
    return $ case valueM of
        Nothing    -> constraintDefault cst
        Just value -> case decimal value of
            Right (valueI, _) -> constraintApply cst valueI
            Left _            -> constraintDefault cst

pageConstraint :: Constraint
pageConstraint = Constraint
    { constraintMin     = 0
    , constraintMax     = 262144
    , constraintDefault = 0
    }

-- List of resources
getResourceListR :: Handler Html
getResourceListR = do
    pageNumber <- lookupGetParamInt "page" pageConstraint
    let resultsPerPage = 30
        reqFilter = [ Asc ResourceWeight
                    , Asc ResourceResname
                    , LimitTo resultsPerPage
                    , OffsetBy (pageNumber * resultsPerPage)
                    ]

    resources <- runDB $ selectList [] reqFilter
    userIdM <- maybeAuthId

    defaultLayout $ do
        setTitle "Liste des ressources"
        $(widgetFile "resource/list")

