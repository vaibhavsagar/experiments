import DynFlags
import GHC
import GHC.Paths
import MonadUtils (liftIO)
import Unsafe.Coerce

main :: IO ()
main = defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
         -- we have to call 'setSessionDynFlags' before doing
         -- everything else
        dflags <- getSessionDynFlags
        -- If we want to make GHC interpret our code on the fly, we
        -- ought to set those two flags, otherwise we
        -- wouldn't be able to use 'setContext' below
        setSessionDynFlags $ dflags { hscTarget = HscAsm
                                    -- , ghcLink   = LinkInMemory
                                    }
        setTargets =<< sequence [guessTarget "Test.hs" Nothing]
        load LoadAllTargets
        -- Bringing the module into the context
        importDecl <- IIDecl <$> parseImportDecl "import Prelude"
        setContext
            $ (IIDecl . simpleImportDecl $ mkModuleName "Test") : [importDecl]
        -- evaluating and running an action
        setSessionDynFlags dflags
        act <- unsafeCoerce <$> compileExpr "Prelude.print test"
        liftIO act
