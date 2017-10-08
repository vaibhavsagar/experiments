import Data.Vect

readToBlank : IO (List String)
readToBlank = do line <- getLine
                 if (line == "")
                    then pure []
                    else do lines <- readToBlank
                            pure (line :: lines)

readAndSave : IO ()
readAndSave = do putStrLn "Enter lines (blank line to end):"
                 lines <- readToBlank
                 let lines' = concat $ intersperse "\n" lines
                 putStr "Enter filename: "
                 filename <- getLine
                 writeFile filename lines'
                 pure ()

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do Right h <- openFile filename Read
                           readVectFileHelper h
    where readVectFileHelper : File -> IO (n ** Vect n String)
          readVectFileHelper h = do eof <- fEOF h
                                    case eof of
                                         True => pure (_ ** [])
                                         False => do Right line <- fGetLine h
                                                     (n ** lines) <- readVectFileHelper h
                                                     pure (_ ** (line :: lines))

