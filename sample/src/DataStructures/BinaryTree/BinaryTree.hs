module DataStructures.BinaryTree.BinaryTree (
    BinaryTree(..),
    empty,
    insert,
    search,
    delete,
    inorder,
    preorder,
    postorder,
    height,
    size
) where

-- | 이진 트리 자료구조
data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a)
    deriving (Show, Eq)

-- | 빈 트리 생성
empty :: BinaryTree a
empty = Empty

-- | 이진 탐색 트리에 요소 삽입
insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert x Empty = Node x Empty Empty
insert x (Node val left right)
    | x < val = Node val (insert x left) right
    | x > val = Node val left (insert x right)
    | otherwise = Node val left right  -- 중복은 무시

-- | 이진 탐색 트리에서 요소 검색
search :: Ord a => a -> BinaryTree a -> Bool
search _ Empty = False
search x (Node val left right)
    | x == val = True
    | x < val = search x left
    | otherwise = search x right

-- | 이진 탐색 트리에서 요소 삭제
delete :: Ord a => a -> BinaryTree a -> BinaryTree a
delete _ Empty = Empty
delete x (Node val left right)
    | x < val = Node val (delete x left) right
    | x > val = Node val left (delete x right)
    | otherwise = case (left, right) of
        (Empty, Empty) -> Empty
        (Empty, r) -> r
        (l, Empty) -> l
        (l, r) -> let minVal = findMin r
                  in Node minVal l (delete minVal r)
    where
        findMin (Node v Empty _) = v
        findMin (Node _ l _) = findMin l

-- | 중위 순회 (inorder traversal)
inorder :: BinaryTree a -> [a]
inorder Empty = []
inorder (Node val left right) = inorder left ++ [val] ++ inorder right

-- | 전위 순회 (preorder traversal)
preorder :: BinaryTree a -> [a]
preorder Empty = []
preorder (Node val left right) = [val] ++ preorder left ++ preorder right

-- | 후위 순회 (postorder traversal)
postorder :: BinaryTree a -> [a]
postorder Empty = []
postorder (Node val left right) = postorder left ++ postorder right ++ [val]

-- | 트리의 높이 계산
height :: BinaryTree a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)

-- | 트리의 노드 개수 계산
size :: BinaryTree a -> Int
size Empty = 0
size (Node _ left right) = 1 + size left + size right

