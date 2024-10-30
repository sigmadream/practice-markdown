# Markdown

> 마크다운 사용법을 소개하기 위한 저장소입니다. 해당 문서의 마크다운 관련 원본은 [jinkyukim-me](https://github.com/jinkyukim-me/markdown_ko)님의 저장소를 참고하였습니다. `Pandoc`과 관련된 내용은 부산대학교에서 진행된 2024년 2학기 '공학작문 및 발표'를 위해서 추가하였습니다. 마크다운 관련된 자료가 도움이 되셨길 바랍니다.

## HTML 단점

웹에서 사용하는 태그는 가독성과 작업의 편의성을 저하시킬 수 있습니다.

기존의 출판과 달리 웹에서 찾을 수 있는 콘텐츠는 `HTML`(hyper text markup language)이라는 마크업(markup) 언어로 구성되어 있습니다. HTML은 다양한 요소(제목, 목록, 이미지, 텍스트 등)로 구성되어 있습니다. 해당 구성요소는 태그(tag)를 사용합니다. 이런 태그를 사용해서 내용을 기술하는 작업은 번거로울 수 있습니다. 왜냐하면 태그를 사용하기 위해선 `<`,`>`로 둘러싸인 형태의 태그를 사용해야 하기 때문입니다. 이런 태그를 사용하는 것은 쉽지 않습니다. 또한, 태그를 사용하면 문서의 내용을 읽기 어렵게 만들 수 있습니다.

## 마크다운

HTML에서 사용하는 태그로 인한 문제를 해결하기 위해 마크다운(markdown)이라는 간단한 마크업 언어가 만들어졌습니다. 마크다운은 [존 그루버](https://daringfireball.net/projects/markdown/)가 만든 일반 텍스트 서식을 지정하는 간단한 방법입니다. 웹용 문서의 서식을 지정하는 더 효율적이고 간략한 방법이 마크다운(markdown)입니다.

마크다운을 사용하면 웹용 문서의 서식을 빠르게 지정할 수 있으며, 언제든지 HTML로 변환할 수 있습니다. 마크다운은 태그에 의존하는 대신 키보드의 단순 기호를 사용하여 서식을 지정합니다. 예를 들어 해시 기호(`#`)는 제목을 나타냅니다. 마크다운으로 목록, 들여쓰기 텍스트 등을 만들 수도 있습니다. 수많은 HTML 태그를 입력하는 것보다 훨씬 간편합니다!

## 마크다운의 장점

마크다운에는 여러 가지 장점이 있습니다. 일반 텍스트이므로 모든 운영 체제, 모든 모바일 장치 또는 웹에서 문서 작업을 할 수 있습니다. 마크다운을 사용하면 적은 노력으로 효율적으로 문서를 작성하고 서식을 지정할 수 있습니다. 무엇보다 내용과 형식을 손쉽게 분리할 수 있다는 점도 장점이라 할 수 있습니다.

## 마크다운 문법

좋든 나쁘든, 마크다운을 사용하면 다양한 방식으로 특정 종류의 서식을 적용할 수 있습니다. 제가 가장 효율적으로 사용할 수 있는 서식을 알려드리겠습니다.

> 시작할 준비가 되셨나요? 좋아요! 이제 시작해보죠.

## Table of contents(목차)

[01. Headers(제목)](#01-headers제목)<br>
[02. Emphasis(강조)](#02-emphasis강조)<br>
[03. Block quotes(인용)](#03-block-quotes인용)<br>
[04. Lists(목록)](#04-lists목록)<br>
[05. Backslash Escapes(이스케이프 시퀀스)](#05-backslash-escapes이스케이프-시퀀스)<br>
[06. Images(이미지)](#06-images이미지)<br>
[07. Links(Anchor, 링크)](#07-linksanchor-링크)<br>
[08. Fenced Code Blocks(코드 블럭)](#08-fenced-code-blocks코드-블럭)<br>
[09. Task List(체크 리스트)](#09-task-list체크-리스트)<br>
[10. Horizontal Rules(수평선)](#10-horizontal-rules수평선)<br>
[11. Emoji(이모지)](#11-emoji이모지)<br>
[12. Table(테이블)](#12-table테이블)<br>
[13. Line Breaks(줄바꿈)](#13-line-breaks줄바꿈)<br>

---

## 01. Headers(제목)

- `#`으로 시작하는 문자열
- `#`은 최대 여섯개까지 가능(`H1` ~ `H6`)
- `#`이 늘어날때마다 제목의 크기(스케일)이 낮아짐
- `H1`은 `===`, `H2`는 `---` 가능

### Syntax 마크다운 사용법

```
This is an H1
===
This is an H2
---
# This is an H1
## This is an H2
### This is an H3
#### This is an H4
##### This is an H5
###### This is an H6
```

### 실행결과

# This is an H1<br>

## This is an H2<br>

# This is an H1(parts)<br>

## This is an H2(chapters)<br>

### This is an H3(page)<br>

#### This is an H4(sub)<br>

##### This is an H5(subsub)<br>

###### This is an H6(p)<br>

---

## 02. Emphasis(강조)

- 기울여 쓰기(`italic`) : `*` 또는 `_`로 감싼 텍스트
- 두껍게 쓰기(`bold`) : `**` 또는 `__`로 감싼 텍스트
- 취소선(`strikethrough`) : `~~`로 감싼 텍스트
- 이탤릭체와 두껍게를 같이 사용할 수 있음

### Syntax 마크다운 사용법

```
*This text will be italic*
_This will also be italic_
**This text will be bold**
__This will also be bold__
~~This is canceled~~
*You **can** combine them*
```

### 실행결과

_This text will be italic_<br>
_This will also be italic_<br>
**This text will be bold**<br>
**This will also be bold**<br>
~~This is canceled~~<br>
_You **can** combine them_<br>

---

## 03. Block quotes(인용)

- `>`으로 시작하는 텍스트
- `>`는 3개까지 가능
- `1개`는 인용문
- `2개`는 인용문 안에 인용문
- `3개`는 인용문 안에 인용문 안에 인용문

### Syntax 마크다운 사용법

```
As Grace Hopper said:
> I’ve always been more interested in the future than in the past.
> This is a first blockquote.
> > This is a second blockquote.
> > > This is a third blockquote.
```

### 실행결과

As Grace Hopper said:

> I’ve always been more interested in the future than in the past.
> This is a first blockquote.
>
> > This is a second blockquote.
> >
> > > This is a third blockquote.

---

## 04. Lists(목록)

### 04-1. Unordered lists 순서가 없는 목록

- `*`, `+`, `-` 를 이용해서 순서가 없는 목록을 만들 수 있음
- 들여쓰기를 하면 모양이 바뀜

### 04-2. Ordered lists 순서가 있는 목록

- 숫자를 기입하면 순서가 있는 목록
- 들여쓰기를 하면 모양이 바뀜

### Syntax 마크다운 사용법

```
* Item 1
* Item 2
	* Item 1
	* Item 2
		* Item 1
		* Item 2
1. Item 1
2. Item 2
3. Item 3
	1. Item 1
	2. Item 2
	3. Item 3
		1. Item 1
		2. Item 2
		3. Item 3
```

### 실행결과

- Item 1
- Item 2
  - Item 1
  - Item 2
    - Item 1
    - Item 2

1. Item 1
2. Item 2
3. Item 3
   1. Item 1
   2. Item 2
   3. Item 3
      1. Item 1
      2. Item 2
      3. Item 3

---

## 05. Backslash Escapes(이스케이프 시퀀스)

- 특수문자를 표현할 때, 표시될 문자 앞에 `\`를 넣고 특수문자를 적으면 됨
- 주의할 점은 앞과 뒤에가 형식이 똑같이 백슬래쉬 뒤에 특수문자입니다. `감싸는 형태가 아님`
- 백슬래쉬는 아래의 특수문자를 표현할 수 있음
  \_ \ `backslash`, \` `backtick`, \* `asterisk`, \_ `underscore`, \{} `curly braces`, \[] `square brackets`, \() `parentheses`, \# `hash mark`, \+ `plus sign`, \- `minus sign (hyphen)`, \. `dot`, \! `exclamation mark`

### Syntax 마크다운 사용법

```
\*literal asterisks\*
\#hash mark\#
\[squre brackets\]
```

### Demonstration 실행결과

\*literal asterisks\*<br>
\#hash mark\#<br>
\[squre brackets\]<br>

---

## 06. Images(이미지)

- `<img>`로 변환
- 링크와 비슷하지만 앞에 `!`가 붙음
- 인라인 이미지 \![alt text](/test.png\)
- 링크 이미지 \![alt text](image_URL\)
- 이미지의 사이즈를 변경하기 위해서는 `<img width="OOOpx" height="OOOpx"></img>`와 같이 표현

### Syntax 마크다운 사용법

```
![alt 토마토](/img/tomato.jpg)
![alt man](/img/man_laptop.jpg)
![alt Concrete Buildings](https://github.com/sigmadream/practice-markdown/blob/main/img/linux.jpg)
```

### 실행결과

![alt 윈도우](/img/windows.jpg)
![alt macOS](/img/macos.jpg)
![alt 리눅스](https://raw.githubusercontent.com/sigmadream/practice-markdown/main/img/linux.jpg)

---

## 07. Links(Anchor, 링크)

## 07-1. External Links

- 인라인 링크: [링크](http://example.com "링크 제목")
- url 링크: <example.com>, <example@example.com>; 꺽쇠 괄호 없어도 자동으로 링크를 사용

### Syntax 마크다운 사용법

```
[Google](http://www.google.com "구글")
[Naver](http://www.naver.com "네이버")
[Github](http://www.github.com "깃허브")
구글 www.google.com; 꺽쇠없음
네이버 <www.naver.com>; 꺽쇠있음
My mail <test@test.com>
```

### 실행결과

[Google](http://www.google.com "구글")<br>
[Naver](http://www.naver.com "네이버")<br>
[Github](http://www.github.com "깃허브")<br>
구글 www.google.com <br>
네이버 <www.naver.com><br>
My mail <test@test.com><br>

## 07-2. Internal Links

- [보여지는 내용](#이동할 헤드(제목))
- 괄호 안의 링크를 쓸 때는 띄어쓰기는 `-`로 연결, 영어는 모두 소문자로 작성

### Syntax 마크다운 사용법

```
[1. Headers 헤더](#1-headers-헤더)
[2. Emphasis 강조](#2-emphasis-강조)
[3. Blockquotes 인용](#3-blockquotes-인용)
```

### 실행결과

[01. Headers 헤더](#1-headers)<br>
[02. Emphasis 강조](#2-emphasis)<br>
[03. Block quotes 인용](#3-blockquotes)<br>

---

## 08. Fenced Code Blocks(코드 블럭)

- 간단한 인라인 코드는 텍스트를 앞뒤로 \`기호로 감싸면 됨
- \`\`\` 혹은 ~~~ 코드.
- 첫 줄과 마지막 줄에 Back quote ( \` ) 또는 물결( ~ ) 3개 삽입
- 코드가 여러 줄인 경우, 줄 앞에 공백 네 칸을 추가
- \`\`\` 옆에 언어를 지정해주면 syntax color가 적용

### Syntax 마크다운 사용법

````
    ```
    This is code blocks.
    ```
    ~~~
    This is code blocks.
    ~~~
    ```javascript
    function test() {
     console.log("look ma’, no spaces");
    }
    ```
````

### 실행결과

```
This is code blocks.
```

```
This is code blocks.
```

```javascript
function test() {
  console.log("look ma’, no spaces");
}
```

---

## 09. Task List(체크 리스트)

- 줄 앞에 `- [x]`를 써서 완료된 리스트 표시
- 줄 앞에 `- [ ]`를 써서 미완료된 리스트 표시
- 체크 안에서 강조 외에 여러 기능을 사용할 수 있으며, Github에서 활용 가능

### Syntax 마크다운 사용법

```
- [x] this is a complete item
- [ ] this is an incomplete item
- [x] @mentions, #refs, [links](), **formatting**, and <del>tags</del> supported
- [x] list syntax required (any unordered or ordered list supported)
```

### 실행결과

- [x] this is a complete item
- [ ] this is an incomplete item
- [x] @mentions, #refs, [links](), **formatting**, and <del>tags</del> supported
- [x] list syntax required (any unordered or ordered list supported)

---

## 10. Horizontal Rules(수평선)

- \- 또는 \* 또는 \_ 을 3개 이상 작성.
- 단, -을 사용할 경우 header로 인식할 수 있으니 이 전 라인은 비워두어야 함

### Syntax 마크다운 사용법

```
* * *
***
*****
- - -
-------------------
```

### 실행결과

---

---

---

---

---

## 11. Emoji(이모지)

- 마크다운을 이용해 이모티콘을 표현가능
- 깃허브도 적용가능
- 더 많은 리스트는 아래의 사이트로 방문
- [Emoji Cheat Sheet](www.emoji-cheat-sheet.com)

### Syntax 마크다운 사용법

```
GitHub supports emoji!
:+1: :sparkles: :camel: :tada:
:rocket: :metal: :octocat:
```

### 실행결과

GitHub supports emoji!
:+1: :sparkles: :camel: :tada:
:rocket: :metal: :octocat:

---

## 12. Table(테이블)

- 헤더와 셀을 구분할 때 3개 이상의 `-`(hyphen/dash) 기호가 필요
- 헤더 셀을 구분하면서 :(Colons) 기호로 셀(열/칸) 안에 내용을 정렬할 수 있음
- 가장 좌측과 가장 우측에 있는 |(vertical bar) 기호는 생략 가능

### Syntax 마크다운 사용법

```
테이블 생성

헤더1|헤더2|헤더3|헤더4
---|---|---|---
셀1|셀2|셀3|셀4
셀5|셀6|셀7|셀8
셀9|셀10|셀11|셀12

테이블 정렬

헤더1|헤더2|헤더3
:---|:---:|---:
Left|Center|Right
1|2|3
4|5|6
7|8|9
```

### 실행결과

- 테이블 생성

| 헤더1 | 헤더2 | 헤더3 | 헤더4 |
| ----- | ----- | ----- | ----- |
| 셀1   | 셀2   | 셀3   | 셀4   |
| 셀5   | 셀6   | 셀7   | 셀8   |
| 셀9   | 셀10  | 셀11  | 셀12  |

- 테이블 정렬

| 헤더1 | 헤더2  | 헤더3 |
| :---- | :----: | ----: |
| Left  | Center | Right |
| 1     |   2    |     3 |
| 4     |   5    |     6 |
| 7     |   8    |     9 |

---

## 13. Line Breaks(줄바꿈)

- `<br>`를 활용해서 줄바꿈을 할 수 있음

### Syntax 마크다운 사용법

```
言葉はまるで雪の結晶
말은 마치 눈의 결정 <br>
君にプレゼントしたくても
너에게 선물하고 싶어도 <br>
```

### 실행결과

言葉はまるで雪の結晶
말은 마치 눈의 결정 <br>
君にプレゼントしたくても
너에게 선물하고 싶어도 <br>

# Pandoc

Pandoc은 마크다운을 다른 문서 형식으로 변환하는 데 사용할 수 있는 무료 오픈 소스 도구입니다. Pandoc은 다음과 같은 다양한 형식으로 변환할 수 있습니다.

- HTML
- Markdown
- LaTeX
- PDF
- EPUB

## Pandoc의 설치

Pandoc은 다양한 운영 체제에서 사용할 수 있습니다. [Pandoc의 설치](https://pandoc.org/installing.html)를 참고하여 설치하시길 바랍니다.

```
// Windows
$ winget install --source winget --exact --id JohnMacFarlane.Pandoc
$ choco install pandoc
$ scoop install pandoc

// macOS
$ brew install pandoc
```

## Markdown을 HTML로 변환

```
$ pandoc -s README.md -c README.css --template template.html --toc --toc-depth=2 -o README.html
```
