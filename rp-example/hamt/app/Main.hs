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
import Data.Functor ((<$), void)
import Control.Monad.Fix (MonadFix)
import Control.Lens ((^.))

import Language.Javascript.JSaddle

data Op = InsertTree | DeleteTree

main = mainWidgetWithHead widgetHead $ el "div" $ do
  key <- valueInput "key"
  val <- valueInput "value"
  b <- button "insert"
  d <- button "delete"
  let events = leftmost [InsertTree <$ b, DeleteTree <$ d]
  let values = zipDynWith (,) key val
  tree <- foldDyn
    (\((k,v), action) t -> case action of
      InsertTree -> insert k v t
      DeleteTree -> delete k t)
    None
    (attachPromptlyDyn values events)
  let hamtDot = fmap (pack . dotFromHAMT) tree
  text " = "
  graphVizDiv <- toJSVal . _element_raw . fst <$> el' "div" blank
  el "pre" $ dynText hamtDot
  performEvent_ $ ffor (updated hamtDot) $ \dot -> liftJSM $ do
    viz <- new (jsg @Text "Viz") ()
    render <- viz ^. js1 @Text "renderSVGElement" dot
    andThen <- render ^. js1 @Text "then" (fun $ \_ _ [element] -> do
      outerHTML <- element ! ("outerHTML" :: Text)
      graphVizDiv ^. jss @Text "innerHTML" outerHTML)
    void $ andThen ^. js1 @Text "catch" (fun $ \_ _ [err] -> void $
      jsg @Text "console" >>= \console -> console ^. js1 @Text "log" err)
  where
    widgetHead :: (DomBuilder t m) => m ()
    widgetHead = do
      script "https://cdn.jsdelivr.net/npm/viz.js@2.1.2/viz.min.js"
      script "https://cdn.jsdelivr.net/npm/viz.js@2.1.2/full.render.min.js"
    script src = elAttr "script" ("type" =: "text/javascript" <> "src" =: src) blank

valueInput :: (DomBuilder t m, MonadFix m) => Text -> m (Dynamic t String)
valueInput placeholder = do
  let initAttrs = ("type" =: "string") <> (style False) <> ("placeholder" =: placeholder)
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
