data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x tree@(Node left val right) = case compare x val of
                                      LT => let left' = insert x left in
                                                Node left' val right
                                      EQ => tree
                                      GT => let right' = insert x right in
                                                Node left val right'

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : Ord a => Tree a -> List a
treeToList Empty = []
treeToList (Node left x right)
    = let
        leftList = treeToList left
        rightList = treeToList right
    in leftList ++ [x] ++ rightList
