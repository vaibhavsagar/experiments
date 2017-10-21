module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
    constructor MkData
    schema : Schema
    size : Nat
    items : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size items) y = MkData schema _ (addToData items)
    where
        addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
        addToData [] = [y]
        addToData (x :: xs) = x :: addToData xs

data Command : Schema -> Type where
    SetSchema : (newschema : Schema) -> Command schema
    Add : SchemaType schema -> Command schema
    Get : Maybe Integer -> Command schema
             -- | Search String
    Size : Command schema
    Quit : Command schema

getQuoted : List Char -> Maybe (String, String)
getQuoted ('"' :: xs) = case span (/= '"') xs of
                             (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                             _ => Nothing
getQuoted _ = Nothing

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
parsePrefix SInt input = case span isDigit input of
                          ("", rest) => Nothing
                          (num, rest) => Just (cast num, ltrim rest)
parsePrefix SChar input = case span (/= ' ') (unpack input) of
                               (c :: [], ' ' :: rest) => Just (c, pack rest)
                               _ => Nothing
parsePrefix (schemal .+. schemar) input = case parsePrefix schemal input of
                                               Nothing => Nothing
                                               Just (l_val, input') => case parsePrefix schemar input' of
                                                                            Nothing => Nothing
                                                                            Just (r_val, input'') => Just ((l_val, r_val), input'')
parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs)
    = case xs of
           [] => Just SString
           _ => case parseSchema xs of
                     Nothing => Nothing
                     Just xs_sch => Just (SString .+. xs_sch)
parseSchema ("Int" :: xs)
    = case xs of
           [] => Just SInt
           _ => case parseSchema xs of
                     Nothing => Nothing
                     Just xs_sch => Just (SInt .+. xs_sch)
parseSchema ("Char" :: xs)
    = case xs of
           [] => Just SChar
           _ => case parseSchema xs of
                     Nothing => Nothing
                     Just xs_sch => Just (SChar .+. xs_sch)
parseSchema _ = Nothing

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                              (Just (res, "")) => Just res
                              (Just _) => Nothing
                              Nothing => Nothing

parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "add" rest = case parseBySchema schema rest of
                                      Nothing => Nothing
                                      Just restok => Just (Add restok)
parseCommand schema "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => case (unpack val) of
                                           [] => Just (Get Nothing)
                                           _ => Just (Get (Just (cast val)))
-- parseCommand "search" pre = Just (Search pre)
parseCommand schema "size" "" = Just Size
parseCommand schema "quit" "" = Just Quit
parseCommand schema "schema" rest
    = case parseSchema (words rest) of
           Nothing => Nothing
           Just schemaok => Just (SetSchema schemaok)
parseCommand _ _ _ = Nothing

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z => Just (MkData schema _ [])
                              (S k) => Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case (span (/= ' ')) input of
                   (cmd, args) => parseCommand schema cmd (ltrim args)

display : SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = SChar} item = show item
display {schema = (x .+. y)} (iteml, itemr)
        = display iteml ++ ", " ++ display itemr

displayAll : Vect n (SchemaType schema) -> String
displayAll {n} xs = unlines . toList $ map (displayEntry xs) (fromList [0..n])
    where displayEntry :  Vect n (SchemaType schema) -> Nat -> String
          displayEntry xs k = case natToFin k n of
                                   Nothing => ""
                                   Just id => show k ++ ": " ++ (display (index id xs))

getEntry : (pos : Integer) -> (store : DataStore) ->
           Maybe (String, DataStore)
getEntry pos store
    = let store_items = items store in
          case integerToFin pos (size store) of
               Nothing => Just ("Out of range\n", store)
               Just id => Just (display (index id store_items) ++ "\n", store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input
    = case parse (schema store) input of
           Nothing => Just ("Invalid command\n", store)
           (Just (Add item)) =>
                    Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
           (Just (Get Nothing)) => Just (displayAll (items store), store)
           (Just (Get (Just x))) => getEntry x store
           -- (Just (Search p)) => let
           --         its = items store
           --         matchIndices = findIndices (?infix p) its
           --         matches = map (\i => (cast (finToInteger i)) ++ ": " ++ (?disp (index i its) ++ "\n") matchIndices
           --      in Just (concat matches, store)
           (Just Size) => Just (show (size store) ++ "\n", store)
           (Just (SetSchema schema')) =>
                case setSchema store schema' of
                     Nothing => Just ("Can't update schema\n", store)
                     Just store' => Just ("OK\n", store')
           (Just Quit) => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
