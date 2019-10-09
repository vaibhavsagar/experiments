{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

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

data Op = InsertTree | DeleteTree

main = mainWidget $ el "div" $ do
  key <- valueInput
  val <- valueInput
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
  let resultText = fmap (pack . ppShow) tree
  text " = "
  el "pre" $ dynText resultText

valueInput :: (MonadWidget t m, MonadFix m) => m (Dynamic t String)
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
