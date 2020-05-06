{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Reflex.Dom
import Language.Haskell.HLint
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)

main = mainWidget $ el "div" $ do
  t  <- _textAreaElement_value <$> textAreaElement def
  ideas <- performEvent $ ffor (updated t) $ \text -> liftIO $
    lint (T.unpack text)
  ideasDyn <- holdDyn [] ideas
  el "div" $
    dynText $ T.pack . show <$> ideasDyn

-- Initialise hlint settings
{-# NOINLINE settings #-}
settings = unsafePerformIO autoSettings

lint :: String -> IO [Idea]
lint code = do
  let (flags, classify, hint) = settings

  parsed <- parseModuleEx flags "-" (Just code)

  -- create 'ideas'
  let ideas = case parsed of
        Left _ -> []
        Right mods -> applyHints classify hint [mods]

  return ideas
