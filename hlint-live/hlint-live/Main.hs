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
import Reflex.Utils
import Reflex.CodeMirror
import Text.RawString.QQ
import qualified Data.Aeson as A

main :: IO ()
main = mainWidget main_
    where
        main_ :: forall t m. MonadWidget t m => m ()
        main_ = do
            headD <- head_
            whenLoaded [headD] blank body
            return ()

head_ :: forall t m. MonadWidget t m => m (Dynamic t Bool)
head_ = do
    s1Ds <- sequence [ script "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.54.0/codemirror.min.js"
                     , css    "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.54.0/codemirror.min.css"
                     ]
    el "style" $ text [r|
      .CodeMirror {
        border: 1px solid black;
      }
    |]
    whenLoaded s1Ds blank $ do
        sequence [ script "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.54.0/mode/haskell/haskell.min.js"
                 ]
        return ()

body :: MonadWidget t m => m ()
body = el "div" $ do
    t <- codemirror config never never
    ideas <- performEvent $ ffor t $ \text -> liftIO $
        lint (T.unpack text)
    ideasDyn <- holdDyn [] ideas
    simpleList ideasDyn $ \ideaDyn -> do
        idea <- sample $ current ideaDyn
        ideaWidget idea
    el "div" $ do
        (dynText $ T.pack . displayLint <$> ideasDyn)
    where
        config :: Configuration
        config
            = def
                { _configuration_theme = Just "default"
                , _configuration_mode = Just $ A.String "haskell"
                }


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

