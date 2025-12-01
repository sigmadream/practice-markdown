# BinaryTree (이진 트리) - 하스켈 구현

> 이 노트는 하스켈로 이진 탐색 트리를 구현하는 방법을 소개하며, 하스켈과 C++의 자료구조 표현 및 함수 작성 방식의 차이, 그리고 함수형 프로그래밍의 장점을 간략히 비교 및 요약합니다.

## 개요

하스켈로 구현한 이진 탐색 트리(Binary Search Tree) 자료구조입니다. 재귀적 데이터 구조와 패턴 매칭을 활용한 함수형 구현입니다.

## 하스켈 vs C++ 차이점

### 1. 재귀적 데이터 구조의 표현

- 하스켈

```haskell
data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a)
-- 타입 정의 자체가 재귀적이고 선언적
```

- C++

```cpp
template<typename T>
struct Node {
    T data;
    Node* left;
    Node* right;
    // 포인터와 명시적 메모리 관리 필요
};
```

- 하스켈: 타입 시스템에 통합된 재귀적 구조, 선언적
- C++: 포인터 기반, 명시적 메모리 관리 필요

### 2. 패턴 매칭과 구조 분해

- 하스켈

```haskell
insert x Empty = Node x Empty Empty
insert x (Node val left right)
    | x < val = Node val (insert x left) right
    | x > val = Node val left (insert x right)
-- 패턴 매칭으로 구조 분해와 조건 처리 동시에
```

- C++

```cpp
Node* insert(Node* root, T x) {
    if (root == nullptr) {
        return new Node(x, nullptr, nullptr);
    }
    if (x < root->data) {
        root->left = insert(root->left, x);
    } else if (x > root->data) {
        root->right = insert(root->right, x);
    }
    return root;
}
```

- 하스켈: 패턴 매칭으로 간결하고 안전한 구조 분해
- C++: 명시적 null 체크와 포인터 조작

### 3. 불변성과 함수형 업데이트

- 하스켈

```haskell
insert x (Node val left right) = Node val (insert x left) right
-- 새로운 트리 반환, 원본은 변경되지 않음
```

- C++

```cpp
// 가변적: 기존 노드 수정
root->left = insert(root->left, x);
// 또는 불변적: 새 트리 반환 (비효율적)
```

- 하스켈: 불변성으로 안전하고 예측 가능
- C++: 가변성으로 효율적이지만 위험할 수 있음

### 4. 타입 클래스와 제약 조건

- 하스켈

```haskell
insert :: Ord a => a -> BinaryTree a -> BinaryTree a
-- Ord 타입 클래스로 비교 가능한 타입만 허용
```

- C++

```cpp
template<typename T>
// T가 < 연산자를 지원해야 함 (컴파일 타임 체크)
// 하지만 명시적 제약 없음
```

- 하스켈: 타입 클래스로 명시적 제약, 컴파일 타임 보장
- C++: 템플릿으로 유연하지만 제약이 암묵적

### 5. 순회의 표현력

- 하스켈

```haskell
inorder Empty = []
inorder (Node val left right) = inorder left ++ [val] ++ inorder right
-- 재귀적 정의가 수학적 정의와 일치
```

- C++

```cpp
void inorder(Node* root, vector<T>& result) {
    if (root == nullptr) return;
    inorder(root->left, result);
    result.push_back(root->data);
    inorder(root->right, result);
}
// 부작용 있는 함수, 결과를 인자로 전달
```

- 하스켈: 순수 함수로 선언적이고 수학적
- C++: 부작용 있는 함수, 명령형 스타일

### 6. 메모리 관리

- 하스켈

```haskell
-- 가비지 컬렉터가 자동 관리
-- 불변성으로 구조적 공유 가능
let tree1 = insert 5 empty
let tree2 = insert 3 tree1
-- tree1과 tree2는 공유 가능
```

- C++

```cpp
// 수동 메모리 관리
Node* tree1 = insert(nullptr, 5);
Node* tree2 = insert(tree1, 3);
// tree1이 수정됨 (가변적) 또는 새로 할당 (비효율적)
```

- 하스켈: 자동 메모리 관리, 구조적 공유
- C++: 명시적 메모리 관리, 복사 오버헤드 또는 위험한 공유

### 7. 삭제 연산의 복잡성

- 하스켈

```haskell
delete x (Node val left right)
    | otherwise = case (left, right) of
        (Empty, Empty) -> Empty
        (Empty, r) -> r
        (l, Empty) -> l
        (l, r) -> let minVal = findMin r
                  in Node minVal l (delete minVal r)
-- 패턴 매칭으로 모든 경우 명시적 처리
```

- C++

```cpp
Node* deleteNode(Node* root, T x) {
    if (root == nullptr) return nullptr;
    // 여러 if-else로 분기 처리
    // 메모리 해제 관리 필요
}
```

- 하스켈: 패턴 매칭으로 모든 경우를 타입 시스템으로 보장
- C++: 명시적 조건문, 메모리 해제 관리 필요

### 8. 부작용 없는 순수 함수

- 하스켈

```haskell
-- 모든 트리 연산은 순수 함수
-- 같은 입력에 항상 같은 출력
-- 멀티스레드 안전
```

- C++

```cpp
// 상태 변경 가능
// 동시성 제어 필요
// 부작용으로 인한 버그 가능
```

- 하스켈: 순수 함수로 동시성 안전, 테스트 용이
- C++: 상태 변경으로 동시성 문제, 동기화 필요

## 성능 특성

이진트리의 주요 연산(삽입, 탐색, 삭제)의 시간 복잡도는 트리의 높이(height) $h$에 따라 결정됩니다. 높이 $h$는 트리의 구조에 따라 달라지며, 노드 개수를 $n$이라 할 때 다음과 같이 평균/최악의 높이를 수식으로 유도할 수 있습니다.

### 1. 평균적 경우 (평균 높이)

이진 트리가 평형(balanced) 에 가깝게 유지될 경우, 높이는 아래와 같습니다.

$$
h \approx \log_2 n
$$

따라서 삽입/탐색/삭제 모두 평균적으로 $O(\log n)$의 시간 복잡도를 가집니다.

### 2. 최악의 경우 (worst-case height)

이진 트리가 한쪽으로 쏠린 편향된 트리(skewed tree)로 구성될 수 있씁니다.

$$
h = n - 1
$$

이 때, 높이가 노드 수에 비례합니다. 이 경우 성능은 $O(n)$이 됩니다.

#### 수식적 도출 과정

1. 최악의 경우

    - 한쪽 자식만 계속 추가: 트리는 연결 리스트와 같음  
    - 높이: $h = n - 1$  
    - 시간 복잡도: $O(n)$

2. 평균/이상적 경우

    - 완전이진트리로 가정  
    - 트리 높이 $h$는 $2^h \ge n$을 만족 (높이 $h$에서 최대 $2^h-1$개 노드를 가질 수 있으므로)

$$
\begin{align*}
n \leq 2^h & = \log_2 n \leq h \\ 
& = h \geq \log_2 n \\
& = h = O(\log n)
\end{align*}
$$

3. 정리

    - 평균적 수행 시간: $O(\log n)$  
    - 최악 수행 시간: $O(n)$

이러한 수식 유도를 통해, 이진 트리의 연산 성능은 트리의 균형도에 크게 의존한다는 것을 알 수 있습니다.

## 사용 예제

```haskell
import DataStructures.BinaryTree

-- 트리 생성 및 조작
let tree = insert 5 $ insert 3 $ insert 7 empty
inorder tree  -- [3,5,7]
search 3 tree  -- True
let tree2 = delete 3 tree
inorder tree2  -- [5,7]
```

## 장단점

### 하스켈의 장점

- 타입 안전성: 컴파일 타임에 많은 오류 방지
- 표현력: 수학적 정의와 일치하는 선언적 코드
- 안전성: 불변성으로 버그 감소, 동시성 안전
- 테스트: 순수 함수로 테스트와 검증 용이

### 하스켈의 단점

- 성능: GC로 인한 지연, 불변성으로 인한 오버헤드
- 학습 곡선: 함수형 패러다임 이해 필요
- 실시간성: GC pause로 인한 지연

### C++의 장점

- 성능: 직접적인 메모리 제어, 최적화 가능
- 제어: 메모리와 실행 흐름 완전 제어
- 성숙도: 광범위한 라이브러리와 도구

### C++의 단점

- 안전성: 메모리 오류, null pointer 위험
- 복잡성: 수동 메모리 관리의 복잡성
- 유지보수: 상태 변경으로 인한 버그 가능성

