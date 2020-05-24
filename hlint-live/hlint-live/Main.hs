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

main = mainWidget $ el "div" $ do
    t  <- _textAreaElement_value <$> textAreaElement def
    ideas <- performEvent $ ffor (updated t) $ \text -> liftIO $ do
        lint (T.unpack text)
    ideasDyn <- holdDyn Nothing ideas
    el "div" $
        dynText $ T.pack . displayLint <$> ideasDyn

displayLint :: Maybe [Idea] -> String
displayLint Nothing = "Initialising..."
displayLint (Just ideas) = show ideas

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

lint :: String -> IO (Maybe [Idea])
lint code = do
    let (flags, classify, hint) = settings

    parsed <- parseModuleEx flags "-" (Just code)

    -- create 'ideas'
    ideas <- case parsed of
        Left _ -> pure (Just [])
        Right mods -> do
            ideasEither <- try @SomeException . evaluate $
                applyHints classify hint [mods]
            case ideasEither of
                Left _ -> pure (Just [])
                Right ideas -> pure (Just ideas)

    pure ideas
