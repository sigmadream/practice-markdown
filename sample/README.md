# 하스켈 기본 자료구조

> 하스켈로 구현한 기본 자료구조 모음입니다. 함수형 프로그래밍의 불변성, 타입 안전성, 패턴 매칭 등의 특징을 활용하여 구현했습니다.

## 개요

이 프로젝트는 하스켈의 함수형 프로그래밍 패러다임을 활용하여 기본 자료구조를 구현한 교육용 프로젝트입니다. 각 자료구조는 불변성(immutability), 타입 안전성, 순수 함수 등의 하스켈의 특징을 보여줍니다.

## 구현된 자료구조

각 자료구조는 개별 폴더에 구현되어 있으며, 상세한 설명과 하스켈 vs C++ 비교가 포함된 README.md가 함께 제공됩니다.

- [X] [BinaryTree (이진 트리)](src/DataStructures/BinaryTree/README.md): 이진 탐색 트리 구현
- [ ] [Graph (그래프)](): 그래프 알고리즘 구현
- [ ] [Network (네트워크)](): 네트워크 알고리즘 구현
- [ ] [Numerical Analysis 수치 해석](): 수치 해석 구현

## 하스켈의 특징

이 프로젝트에서 활용된 하스켈의 주요 특징은 아래와 같습니다.

- 불변성 (Immutability): 모든 연산이 새로운 값을 반환하며 원본을 변경하지 않음
- 타입 안전성: `Maybe` 타입 등을 활용한 컴파일 타임 오류 방지
- 패턴 매칭: 구조 분해와 조건 처리를 동시에 수행
- 순수 함수: 부작용 없는 함수로 테스트와 추론이 용이
- 지연 평가: 필요할 때만 계산을 수행하는 효율적인 평가

## 요구사항

ghcup을 통해 설치된 하스켈 환경에서 동작합니다.

### 설치 방법

Windows (Scoop 사용)

```bash
scoop install ghcup
ghcup tui
```

Linux/macOS

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

### 필수 도구

- GHC (Glasgow Haskell Compiler): 하스켈 컴파일러
- Cabal: 하스켈 패키지 및 빌드 시스템

### 설치 확인

```bash
ghc --version
cabal --version
```

## 프로젝트 구조

```
.
├── haskell-data-structures.cabal  # Cabal 프로젝트 설정 파일
├── README.md                       # 프로젝트 메인 문서
├── src/
│   └── DataStructures/
│       ├── Stack/
│       │   ├── Stack.hs           # 스택 구현
│       │   └── README.md           # 스택 상세 설명 및 하스켈 vs C++ 비교
│       ├── Stack.hs               # 스택 모듈 래퍼
│       ├── Queue/
│       │   ├── Queue.hs           # 큐 구현
│       │   └── README.md           # 큐 상세 설명 및 하스켈 vs C++ 비교
│       ├── Queue.hs               # 큐 모듈 래퍼
│       ├── BinaryTree/
│       │   ├── BinaryTree.hs      # 이진 트리 구현
│       │   └── README.md           # 이진 트리 상세 설명 및 하스켈 vs C++ 비교
│       ├── BinaryTree.hs          # 이진 트리 모듈 래퍼
│       ├── LinkedList/
│       │   ├── LinkedList.hs      # 연결 리스트 구현
│       │   └── README.md           # 연결 리스트 상세 설명 및 하스켈 vs C++ 비교
│       └── LinkedList.hs          # 연결 리스트 모듈 래퍼
├── app/
│   └── Main.hs                     # 예제 실행 파일
└── test/
    └── Test.hs                     # 테스트 파일
```

## 빌드 및 실행

### 프로젝트 빌드

프로젝트를 빌드합니다.

```bash
cabal build
```

### 예제 실행

구현된 자료구조들의 사용 예제를 실행합니다.

```bash
cabal run haskell-data-structures
```

### 테스트 실행

모든 자료구조에 대한 테스트를 실행합니다.

```bash
cabal test
```

또는

```bash
cabal run test
```

### GHCi에서 사용하기

대화형 환경에서 모듈을 로드하여 사용할 수 있습니다.

```bash
cabal repl
```

```haskell
ghci> import DataStructures.Stack
ghci> let stack = push 1 $ push 2 $ push 3 empty
ghci> top stack
Just 1
```

## 사용 예제

### Stack

```haskell
import qualified DataStructures.Stack as Stack

-- 스택 생성 및 조작
let stack = Stack.push 1 $ Stack.push 2 $ Stack.push 3 Stack.empty
Stack.top stack        -- Just 1
Stack.size stack       -- 3
case Stack.pop stack of
    Just (x, rest) -> -- x는 1, rest는 Stack [2,3]
    Nothing -> -- 빈 스택
```

더 자세한 내용은 [Stack README](src/DataStructures/Stack/README.md)를 참고하세요.

### Queue

```haskell
import qualified DataStructures.Queue as Queue

-- 큐 생성 및 조작
let queue = Queue.enqueue 1 $ Queue.enqueue 2 $ Queue.enqueue 3 Queue.empty
Queue.front queue      -- Just 1
Queue.size queue       -- 3
case Queue.dequeue queue of
    Just (x, rest) -> -- x는 1, rest는 나머지 큐
    Nothing -> -- 빈 큐
```

더 자세한 내용은 [Queue README](src/DataStructures/Queue/README.md)를 참고하세요.

### BinaryTree

```haskell
import qualified DataStructures.BinaryTree as Tree

-- 이진 탐색 트리 생성 및 조작
let tree = Tree.insert 5 $ Tree.insert 3 $ Tree.insert 7 Tree.empty
Tree.inorder tree      -- [3,5,7]
Tree.preorder tree     -- [5,3,7]
Tree.search 3 tree     -- True
Tree.height tree       -- 2
let tree2 = Tree.delete 3 tree
Tree.inorder tree2     -- [5,7]
```

더 자세한 내용은 [BinaryTree README](src/DataStructures/BinaryTree/README.md)를 참고하세요.

### LinkedList

```haskell
import qualified DataStructures.LinkedList as List

-- 연결 리스트 생성 및 조작
let list = List.cons 1 $ List.cons 2 $ List.cons 3 List.empty
List.head list         -- Just 1
List.length list       -- 3
let list2 = List.cons 4 $ List.cons 5 List.empty
let combined = List.append list list2
List.reverse combined  -- Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 Empty))))
```

더 자세한 내용은 [LinkedList README](src/DataStructures/LinkedList/README.md)를 참고하세요.

## 하스켈 vs C++ 비교

각 자료구조 폴더의 README.md에는 하스켈과 C++의 차이점이 상세히 설명되어 있습니다.

- 불변성과 가변성: 하스켈의 불변성 vs C++의 가변성
- 타입 안전성: `Maybe` 타입 vs 예외 처리
- 메모리 관리: 자동 GC vs 수동 관리
- 표현력: 함수형 스타일 vs 명령형 스타일
- 패턴 매칭: 구조 분해와 조건 처리
- 성능 특성: 각 구현의 시간/공간 복잡도

## 학습 목적

이 프로젝트는 다음을 목적으로 합니다.

1. 함수형 프로그래밍 이해: 하스켈의 불변성, 순수 함수, 타입 시스템 학습
2. 자료구조 구현: 기본 자료구조를 함수형 스타일로 구현
3. 언어 비교: 하스켈과 C++ 등의 명령형 언어의 차이점 이해
4. 타입 안전성: 컴파일 타임에 오류를 방지하는 타입 시스템 활용

## 라이선스

MIT License

## 기여

이슈나 개선 사항이 있으면 언제든지 제안해주세요!
