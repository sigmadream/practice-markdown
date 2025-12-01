module Main where

import qualified DataStructures.Stack as Stack
import qualified DataStructures.Queue as Queue
import qualified DataStructures.BinaryTree as Tree
import qualified DataStructures.LinkedList as List

main :: IO ()
main = do
    putStrLn "=== 하스켈 기본 자료구조 예제 ===\n"
    
    -- Stack 예제
    putStrLn "1. Stack 예제:"
    let stack1 = Stack.push 1 $ Stack.push 2 $ Stack.push 3 Stack.empty
    putStrLn $ "  스택: " ++ show stack1
    putStrLn $ "  최상단: " ++ show (Stack.top stack1)
    case Stack.pop stack1 of
        Just (x, stack2) -> do
            putStrLn $ "  pop 결과: " ++ show x
            putStrLn $ "  남은 스택: " ++ show stack2
        Nothing -> putStrLn "  스택이 비어있습니다"
    putStrLn ""
    
    -- Queue 예제
    putStrLn "2. Queue 예제:"
    let queue1 = Queue.enqueue 1 $ Queue.enqueue 2 $ Queue.enqueue 3 Queue.empty
    putStrLn $ "  큐: " ++ show queue1
    putStrLn $ "  앞쪽 요소: " ++ show (Queue.front queue1)
    case Queue.dequeue queue1 of
        Just (x, queue2) -> do
            putStrLn $ "  dequeue 결과: " ++ show x
            putStrLn $ "  남은 큐: " ++ show queue2
        Nothing -> putStrLn "  큐가 비어있습니다"
    putStrLn ""
    
    -- BinaryTree 예제
    putStrLn "3. BinaryTree 예제:"
    let tree1 = Tree.insert 5 $ Tree.insert 3 $ Tree.insert 7 $ Tree.insert 2 $ Tree.insert 4 Tree.empty
    putStrLn $ "  트리: " ++ show tree1
    putStrLn $ "  중위 순회: " ++ show (Tree.inorder tree1)
    putStrLn $ "  전위 순회: " ++ show (Tree.preorder tree1)
    putStrLn $ "  높이: " ++ show (Tree.height tree1)
    putStrLn $ "  노드 개수: " ++ show (Tree.size tree1)
    putStrLn $ "  4 검색: " ++ show (Tree.search 4 tree1)
    putStrLn ""
    
    -- LinkedList 예제
    putStrLn "4. LinkedList 예제:"
    let list1 = List.cons 1 $ List.cons 2 $ List.cons 3 List.empty
    putStrLn $ "  리스트: " ++ show list1
    putStrLn $ "  첫 번째 요소: " ++ show (List.head list1)
    putStrLn $ "  길이: " ++ show (List.length list1)
    let list2 = List.cons 4 $ List.cons 5 List.empty
    let list3 = List.append list1 list2
    putStrLn $ "  두 리스트 연결: " ++ show list3
    putStrLn $ "  뒤집기: " ++ show (List.reverse list3)
    putStrLn ""

