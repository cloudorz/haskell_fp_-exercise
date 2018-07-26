data BinaryTree a = 
    Leaf 
  | Node (BinaryTree a) a (BinaryTree a) 
  deriving (Eq, Ord, Show)

-- insert
insert' :: Ord a => a -> BinaryTree a -> BinaryTree a 
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right 
  | b > a = Node left a (insert' b right)

-- map
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b 
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf) 

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay = if mapTree (+1) testTree' == mapExpected 
          then print "yup okay!"
          else error "test failed!"

-- tree to list
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node (Node (Node Leaf 2 Leaf) 3 Leaf) 4 Leaf) 5 (Node (Node Leaf 7 Leaf) 8 (Node Leaf 9 Leaf))

testPreorder :: IO () 
testPreorder = if preorder testTree == [5, 4, 3, 2, 8, 7, 9] 
               then putStrLn "Preorder fine!" 
               else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder = if inorder testTree == [2, 3, 4, 5, 7, 8, 9] 
              then putStrLn "Inorder fine!" 
              else putStrLn "Bad news bears."

testPostorder :: IO () 
testPostorder = if postorder testTree == [2, 3, 4, 7, 9, 8, 5]
                then putStrLn "Postorder fine!" 
                else putStrLn "Bad news bears."

-- fold
-- any traversal order is fine
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b tree = foldr f b (inorder tree)
