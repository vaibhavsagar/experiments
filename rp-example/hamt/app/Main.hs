{-# LANGUAGE RecursiveDo       #-}
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
import Control.Monad (join)

import Language.Javascript.JSaddle

data Op = InsertTree | DeleteTree

main = mainWidgetWithHead widgetHead $ el (pack "div") $ do
  key <- valueInput (pack "key")
  val <- valueInput (pack "value")
  b <- button (pack "insert")
  d <- button (pack "delete")
  let events = leftmost [InsertTree <$ b, DeleteTree <$ d]
  let values = zipDynWith (,) key val
  tree <- foldDyn
    (\((k,v), action) t -> case action of
      InsertTree -> insert k v t
      DeleteTree -> delete k t)
    None
    (attachPromptlyDyn values events)
  let hamtDot = fmap (pack . dotFromHAMT) tree
  text (pack " = ")
  graphVizDiv <- _element_raw . fst <$> el' (pack "div") blank
  el (pack "pre") $ dynText hamtDot
  performEvent_ $ ffor (updated hamtDot) $ \dotText ->
    liftJSM . join $ viz <$> toJSVal graphVizDiv <*> toJSVal dotText
  where
    widgetHead :: (DomBuilder t m) => m ()
    widgetHead = do
      script "https://cdn.jsdelivr.net/npm/viz.js@2.1.2/viz.min.js"
      script "https://cdn.jsdelivr.net/npm/viz.js@2.1.2/full.render.min.js"
    script src = elAttr (pack "script") (pack "type" =: pack "text/javascript" <> pack "src" =: pack src) blank

viz :: JSVal -> JSVal -> JSM ()
viz graphVizDiv dot = do
  viz <- new (jsg "Viz") ()
  render <- viz ^. js1 "renderSVGElement" dot
  andThen <- render ^. js1 "then" (fun $ \_ _ [element] -> do
    outerHTML <- element ! "outerHTML"
    graphVizDiv ^. jss "innerHTML" outerHTML)
  void $ andThen ^. js1 "catch" (fun $ \_ _ [err] -> void $
    jsg "console" >>= \console -> console ^. js1 "log" err)

valueInput :: (DomBuilder t m, MonadFix m) => Text -> m (Dynamic t String)
valueInput placeholder = do
  let initAttrs = (attr "type" =: pack "string") <> (style False) <> (attr "placeholder" =: placeholder)
      color error = pack $ if error then "red" else "green"
      style :: Bool -> Map AttributeName Text
      style error = attr "style" =: (pack "border-color: " <> color error)
      styleChange :: Maybe Double -> Map AttributeName (Maybe Text)
      styleChange result = case result of
        (Just _) -> fmap Just (style False)
        (Nothing) -> fmap Just (style True)
      attr = AttributeName Nothing . pack

  rec
    n <- inputElement $ def
      & inputElementConfig_initialValue .~ pack ""
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ initAttrs
      -- & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ modAttrEv
    let result = fmap unpack $ _inputElement_value n
        -- modAttrEv  = fmap styleChange (updated result)
  return result
