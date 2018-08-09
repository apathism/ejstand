{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module EjStand.HtmlRenderer
  ( renderStanding
  )
where

import qualified Data.Set                      as Set
import           Data.Text                     (splitOn)
import           Data.Text.Lazy                (Text)
import           EjStand.BaseModels
import           EjStand.StandingBuilder       (takeFromSetBy)
import           EjStand.StandingModels
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Hamlet                   (shamletFile)

renderStanding :: Standing -> Text
renderStanding Standing{..} = renderHtml ($(shamletFile "hamlet/main.hamlet"))
