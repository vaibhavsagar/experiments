{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}

import Reflex.Dom
import Language.Haskell.HLint
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Control.Exception
import System.IO.Unsafe (unsafePerformIO)
import Data.FileEmbed
import HLintPath
import Control.Monad (join, void)
import Text.RawString.QQ
import qualified Data.Aeson as A
import Language.Javascript.JSaddle

main :: IO ()
main = do
    mainWidgetWithHead head_ body

head_ :: forall t m. MonadWidget t m => m ()
head_ = do
    script "https://cdn.jsdelivr.net/combine/npm/codemirror@5.62.3,npm/codemirror@5.62.3/mode/haskell/haskell.min.js"
    css    "https://cdn.jsdelivr.net/npm/codemirror@5.62.3/lib/codemirror.min.css"
    el "style" $ text [r|
      .CodeMirror {
        border: 1px solid black;
      }
    |]
    where
        script src = elAttr "script" ("type" =: "text/javascript" <> "src" =: src) blank
        css src = elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: src) blank

body :: MonadWidget t m => m ()
body = el "div" $ do
    _ <- getPostBuild
    tArea <- textAreaElement def
    let tElement = _element_raw . _textAreaElement_element $ tArea
    cm <- liftJSM $ codemirror =<< toJSVal tElement
    onChange <- setupValueListener cm
    ideas <- performEvent $ ffor onChange $ \text -> liftIO $
        lint (T.unpack text)
    ideasDyn <- holdDyn [] ideas
    simpleList ideasDyn $ \ideaDyn -> do
        idea <- sample $ current ideaDyn
        ideaWidget idea
    el "div" $ do
        (dynText $ T.pack . displayLint <$> ideasDyn)

t :: T.Text -> T.Text
t = id

getValueCM :: JSVal -> JSM T.Text
getValueCM cm =
    fromJSValUnchecked =<< (cm # (t "getValue") $ ([] :: [JSVal]))

setupValueListener :: MonadWidget t m => JSVal -> m (Event t T.Text)
setupValueListener codemirrorInstance = do
  pb  <- getPostBuild
  let act callback = liftJSM $ do
        change <- toJSVal $ t "change"
        jscb <- toJSVal <$> asyncFunction $ \_ _ _ -> do
          getValueCM codemirrorInstance >>= liftIO . callback
        void $ codemirrorInstance # (t "on") $ [change, jscb]
  performEventAsync (act <$ pb)

codemirror :: JSVal -> JSM JSVal
codemirror element = do
    cm <- jsg $ t "CodeMirror"
    config <- toJSVal $ A.object
        [ -- ("lineNumbers", A.Bool True)
         ("mode", A.String "haskell")
        ]
    cm # (t "fromTextArea") $ [element, config]

displayLint :: [Idea] -> String
displayLint ideas = unlines (map showIdea ideas)

ideaWidget idea = el "div" $ do
    text $ T.pack $ hintAndSeverity

    where
        hintAndSeverity = if ideaHint idea == ""
            then ""
            else show (ideaSeverity idea) <> ": " <> ideaHint idea

showIdea idea = show idea

-- Initialise hlint settings
{-# NOINLINE settings #-}
settings :: (ParseFlags, [Classify], Hint)
settings = unsafePerformIO autoSettings'

hlintYamlContents :: String
hlintYamlContents = $(embedStringFile hlintYamlPath)

readSettingsFile' :: Maybe FilePath -> String -> IO (FilePath, Maybe String)
readSettingsFile' _ _ = pure ("hlint.yaml", Just hlintYamlContents)

autoSettings' :: IO (ParseFlags, [Classify], Hint)
autoSettings' = do
    (fixities, classify, hints) <- findSettings (readSettingsFile' Nothing) Nothing
    pure (parseFlagsAddFixities fixities defaultParseFlags, classify, hints)

lint :: String -> IO [Idea]
lint code = do
    let (flags, classify, hint) = settings

    parsed <- parseModuleEx flags "-" (Just code)

    -- create 'ideas'
    case parsed of
        Left _ -> pure []
        Right mods -> pure $ applyHints classify hint [mods]
