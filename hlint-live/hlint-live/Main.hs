{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
import Reflex.Dom
import Language.Haskell.HLint
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import Data.FileEmbed
import HLintPath

main = mainWidget $ el "div" $ do
  t  <- _textAreaElement_value <$> textAreaElement def
  ideas <- performEvent $ ffor (updated t) $ \text -> liftIO $
    lint (T.unpack text)
  ideasDyn <- holdDyn [] ideas
  el "div" $
    dynText $ T.pack . show <$> ideasDyn

-- Initialise hlint settings
{-# NOINLINE settings #-}
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
  let ideas = case parsed of
        Left _ -> []
        Right mods -> applyHints classify hint [mods]

  return ideas
