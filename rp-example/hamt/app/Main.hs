{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Reflex
import Reflex.Dom
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (pack, unpack, Text)
import Text.Read (readMaybe)
import HAMT
import Text.Show.Pretty (ppShow)
import Data.Functor ((<$))
import Control.Monad.Fix (MonadFix)

import Language.Javascript.JSaddle

data Op = InsertTree | DeleteTree

main = mainWidgetWithHead widgetHead $ el "div" $ do
  key <- valueInput
  val <- valueInput
  b <- button "insert"
  d <- button "delete"
  viz <- liftJSM $ eval @Text "(function(id, string) { \
    \ var viz = new Viz(); \
    \ viz.renderSVGElement(string) \
      \ .then(function(element) { \
        \ document.getElementById(id).innerHTML = element.outerHTML; \
      \ }) \
      \ .catch(function(error) { \
        \ viz = new Viz(); \
        \ console.log(error); \
      \ }) \
    \ })"
  let events = leftmost [InsertTree <$ b, DeleteTree <$ d]
  let values = zipDynWith (,) key val
  tree <- foldDyn
    (\((k,v), action) t -> case action of
      InsertTree -> insert k v t
      DeleteTree -> delete k t)
    None
    (attachPromptlyDyn values events)
  let resultText = fmap (pack . dotFromHAMT) tree
  text " = "
  elAttr "div" ("id" =: "graph") blank
  el "pre" $ dynText resultText
  performEvent $ ffor (updated tree) $ \t -> liftJSM $ do
    _ <- call viz viz ["graph", pack $ dotFromHAMT t]
    pure ()
  pure ()
  where
    widgetHead :: (DomBuilder t m) => m ()
    widgetHead = do
      elAttr "script" ("type" =: "text/javascript" <> "src" =: "https://cdn.jsdelivr.net/npm/viz.js@2.1.2/viz.min.js") blank
      elAttr "script" ("type" =: "text/javascript" <> "src" =: "https://cdn.jsdelivr.net/npm/viz.js@2.1.2/full.render.min.js") blank

valueInput :: (DomBuilder t m, MonadFix m) => m (Dynamic t String)
valueInput = do
  let initAttrs = ("type" =: "string") <> (style False)
      color error = if error then "red" else "green"
      style error = "style" =: ("border-color: " <> color error)
      styleChange :: Maybe Double -> Map AttributeName (Maybe Text)
      styleChange result = case result of
        (Just _) -> fmap Just (style False)
        (Nothing) -> fmap Just (style True)

  rec
    n <- inputElement $ def
      & inputElementConfig_initialValue .~ ""
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ initAttrs
      -- & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ modAttrEv
    let result = fmap unpack $ _inputElement_value n
        -- modAttrEv  = fmap styleChange (updated result)
  return result
