{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Reflex.Dom
import Language.Haskell.HLint
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Control.Exception
import System.IO.Unsafe (unsafePerformIO)
import Data.FileEmbed
import HLintPath
import Control.Monad (join, void)

main = mainWidget $ el "div" $ do
    t  <- _textAreaElement_value <$> textAreaElement def
    ideas <- performEvent $ ffor (updated t) $ \text -> liftIO $
        lint (T.unpack text)
    ideasDyn <- holdDyn [] ideas
    simpleList ideasDyn $ \ideaDyn -> do
        idea <- sample $ current ideaDyn
        ideaWidget idea
    el "div" $ do
        (dynText $ T.pack . displayLint <$> ideasDyn)

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

