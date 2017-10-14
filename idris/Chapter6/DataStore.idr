module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
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
    Add : SchemaType schema -> Command schema
    Get : Integer -> Command schema
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
parsePrefix (schemal .+. schemar) input = case parsePrefix schemal input of
                                               Nothing => Nothing
                                               Just (l_val, input') => case parsePrefix schemar input' of
                                                                            Nothing => Nothing
                                                                            Just (r_val, input'') => Just ((l_val, r_val), input'')

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
                              True => Just (Get (cast val))
-- parseCommand "search" pre = Just (Search pre)
parseCommand schema "size" "" = Just Size
parseCommand schema "quit" "" = Just Quit
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case (span (/= ' ')) input of
                   (cmd, args) => parseCommand schema cmd (ltrim args)

display : SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (iteml, itemr)
        = display iteml ++ ", " ++ display itemr

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
           (Just (Get x)) => getEntry x store
           -- (Just (Search p)) => let
           --         its = items store
           --         matchIndices = findIndices (?infix p) its
           --         matches = map (\i => (cast (finToInteger i)) ++ ": " ++ (?disp (index i its) ++ "\n") matchIndices
           --      in Just (concat matches, store)
           (Just Size) => Just (show (size store) ++ "\n", store)
           (Just Quit) => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
