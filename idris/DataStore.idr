module Main

import Data.Vect

data DataStore : Type where
    MkData : (size : Nat) ->
             (items : Vect size String) ->
             DataStore

size : DataStore -> Nat
size (MkData size' items) = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) y = MkData _ (addToData items)
    where
        addToData : Vect old String -> Vect (S old) String
        addToData [] = [y]
        addToData (x :: xs) = x :: addToData xs

data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "search" pre = Just (Search pre)
parseCommand "size" "" = Just Size
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case (span (/= ' ')) input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) ->
           Maybe (String, DataStore)
getEntry pos store
    = let store_items = items store in
          case integerToFin pos (size store) of
               Nothing => Just ("Out of range\n", store)
               Just id => Just (index id store_items ++ "\n", store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input
    = case parse input of
           Nothing => Just ("Invalid command\n", store)
           (Just (Add item)) =>
                    Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
           (Just (Get x)) => getEntry x store
           (Just (Search p)) => let
                   its = items store
                   matchIndices = findIndices (Strings.isInfixOf p) its
                   matches = map (\i => (cast (finToInteger i)) ++ ": " ++ (index i its) ++ "\n") matchIndices
                in Just (concat matches, store)
           (Just Size) => Just (show (size store) ++ "\n", store)
           (Just Quit) => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
