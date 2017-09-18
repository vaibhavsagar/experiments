import Data.Function (on)
import Data.List (nubBy)
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
        getTargets
        target <- guessTarget "Test.hs" Nothing
        addTarget target
        getTargets >>= return . nubBy ((==) `on` targetId) >>= setTargets
        load LoadAllTargets
        -- Bringing the module into the context
        importDecl <- parseImportDecl "import Prelude"
        let implicit = IIDecl $ importDecl {ideclImplicit = True}
        setContext
            $ (IIDecl . simpleImportDecl $ mkModuleName "Test") : [implicit]
        -- evaluating and running an action
        setSessionDynFlags dflags
        act <- unsafeCoerce <$> compileExpr "print test"
        liftIO act
