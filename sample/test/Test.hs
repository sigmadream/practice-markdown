module Main where

import qualified DataStructures.BinaryTree as Tree

main :: IO ()
main = do
    putStrLn "=== 자료구조 테스트 ===\n"
    
    -- BinaryTree 테스트
    putStrLn "BinaryTree 테스트:"
    testBinaryTree
    putStrLn ""
    
    putStrLn "모든 테스트 완료!"

testBinaryTree :: IO ()
testBinaryTree = do
    let t1 = Tree.empty :: Tree.BinaryTree Int
    putStrLn $ "  빈 트리 size: " ++ show (Tree.size t1)
    
    let t2 = Tree.insert 5 $ Tree.insert 3 $ Tree.insert 7 $ Tree.insert 2 $ Tree.insert 4 t1
    putStrLn $ "  삽입 후 size: " ++ show (Tree.size t2)
    putStrLn $ "  높이: " ++ show (Tree.height t2)
    putStrLn $ "  중위 순회: " ++ show (Tree.inorder t2)
    putStrLn $ "  3 검색: " ++ show (Tree.search 3 t2)
    putStrLn $ "  10 검색: " ++ show (Tree.search 10 t2)
    
    let t3 = Tree.delete 3 t2
    putStrLn $ "  3 삭제 후 size: " ++ show (Tree.size t3)
    putStrLn $ "  삭제 후 중위 순회: " ++ show (Tree.inorder t3)
