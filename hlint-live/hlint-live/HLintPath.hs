module HLintPath where

import Language.Haskell.HLint
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath ((</>))

hlintYamlPath :: FilePath
hlintYamlPath = let
  hlintDataDir = unsafePerformIO $ getHLintDataDir
  in hlintDataDir </> "hlint.yaml"
