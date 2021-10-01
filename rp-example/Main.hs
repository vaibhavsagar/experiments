{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

import Reflex
import Reflex.Dom
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (pack, unpack, Text)
import Text.Read (readMaybe)
import Control.Applicative ((<*>), (<$>))
import Control.Monad.Fix (MonadFix)

main = mainWidget $ el "div" $ do
  nx <- numberInput
  d <- dropdown Times (constDyn ops) def
  ny <- numberInput
  let values = zipDynWith (,) nx ny
      result = zipDynWith (\o (x,y) -> runOp o <$> x <*> y) (_dropdown_value d) values
      resultText = fmap (pack . show) result
  text " = "
  dynText resultText

numberInput :: (DomBuilder t m, MonadFix m) => m (Dynamic t (Maybe Double))
numberInput = do
  let initAttrs = ("type" =: "number") <> (style False)
      color error = if error then "red" else "green"
      style :: Bool -> Map AttributeName Text
      style error = "style" =: ("border-color: " <> color error)
      styleChange :: Maybe Double -> Map AttributeName (Maybe Text)
      styleChange result = case result of
        (Just _) -> fmap Just (style False)
        (Nothing) -> fmap Just (style True)

  rec
    n <- inputElement $ def
      & inputElementConfig_initialValue .~ "0"
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ initAttrs
      & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ modAttrEv
    let result = fmap (readMaybe . unpack) $ _inputElement_value n
        modAttrEv  = fmap styleChange (updated result)
  return result

data Op = Plus | Minus | Times | Divide deriving (Eq, Ord)

ops :: Map Op Text
ops = Map.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]

runOp :: Fractional a => Op -> a -> a -> a
runOp s = case s of
            Plus -> (+)
            Minus -> (-)
            Times -> (*)
            Divide -> (/)
