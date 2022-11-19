# Markdown Elements

## Table of contents 목차

[1. Headers(제목)](#1-headers-헤더)<br>
[2. Emphasis(강조)](#2-emphasis-강조)<br>
[3. Blockquotes(인용)](#3-blockquotes-인용)<br>
[4. Lists(목록)](#4-lists-목록)<br>
[5. Backslash Escapes(백슬래쉬 이스케이프)](#5-backslash-escapes-백슬래쉬-이스케이프)<br>
[6. Images(이미지)](#6-images-이미지)<br>
[7. Links(Anchor, 링크)](#7-links-anchor-링크)<br>
[8. Fenced Code Blocks(코드 블럭)](#8-fenced-code-blocks-코드-블럭)<br>
[9. Task Lisk(체크 리스트)](#9-task-lisk-체크-리스트)<br>
[10. Horizontal Rules(수평선)](#10-horizontal-rules-수평선)<br>
[11. Emoji(이모티콘)](#11-emoji-이모티콘)<br>
[12. Table(테이블)](#12-table-테이블)<br>
[13. Line Breaks(줄바꿈)](#13-line-breaks-줄바꿈)<br>
[14. Reference(참고 링크)](#14-reference-참고-링크)<br>

## 1. Headers 헤더
* `#`으로 시작하는 문자열
* `#`은 최대 여섯개까지 가능
* `#`이 늘어날때마다 제목의 크기(스케일)이 낮아짐
* H1은 `===`로도 만들 수 있습니다.
* H2는 `---`로도 만들 수 있습니다.

### Syntax 마크다운 사용법
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

### 실행결과
This is an H1<br>
===
This is an H2<br>
---
# This is an H1; 부(parts)에 사용<br>
## This is an H2; 장(chapters)에 사용<br>
### This is an H3; 페이지 섹션에 사용<br>
#### This is an H4; 하위 섹션에 사용<br>
##### This is an H5; 하위 섹션 아래의 하위 섹션에 사용<br>
###### This is an H6; 문단에 사용<br>

## 2. Emphasis
* 기울여 쓰기(italic) : `*` 또는 `_`로 감싼 텍스트
* 두껍게 쓰기(bold) : `**` 또는 `__`로 감싼 텍스트
* 취소선 : `~~`로 감싼 텍스트
* 이탤릭체와 두껍게를 같이 사용할 수 있음

### Syntax 마크다운 사용법
	*This text will be italic*
	_This will also be italic_
	**This text will be bold**
	__This will also be bold__
	~~This is canceled~~
	*You **can** combine them*

### 실행결과
*This text will be italic*<br>
_This will also be italic_<br>
**This text will be bold**<br>
__This will also be bold__<br>
~~This is canceled~~<br>
*You **can** combine them*<br>

## 3. Blockquotes
* `>`으로 시작하는 텍스트
* `>`는 3개까지 가능
* `1개`는 인용문
* `2개`는 인용문 안에 인용문
* `3개`는 인용문 안에 인용문 안에 인용문

### Syntax 마크다운 사용법
    As Grace Hopper said:
    > I’ve always been more interested in the future than in the past.    
    > This is a first blockquote.
    > > This is a second blockquote.
    > > > This is a third blockquote.

### 실행결과
As Grace Hopper said:
> I’ve always been more interested in the future than in the past.
> This is a first blockquote.
> > This is a second blockquote.
> > > This is a third blockquote.

## 4. Lists

### 4.1. Unordered lists 순서가 없는 목록
* `*`, `+`, `-` 를 이용해서 순서가 없는 목록을 만들 수 있음
* 들여쓰기를 하면 모양이 바뀜

### 4.2. Ordered lists 순서가 있는 목록
* 숫자를 기입하면 순서가 있는 목록
* 들여쓰기를 하면 모양이 바뀜

### Syntax 마크다운 사용법
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

### 실행결과
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

## 5. Backslash Escapes
* 특수문자를 표현할 때, 표시될 문자 앞에 `\`를 넣고 특수문자를 적으면 됨
* 주의할 점은 앞과 뒤에가 형식이 똑같이 백슬래쉬 뒤에 특수문자입니다. `감싸는 형태가 아님`
* 백슬래쉬는 아래의 특수문자를 표현할 수 있음
	* \ backslash, \ backtick, * asterisk, _ underscore, {} curly braces, [] square brackets,
() parentheses, # hash mark, + plus sign, - minus sign (hyphen), . dot, ! exclamation mark
### Syntax 마크다운 사용법
	\*literal asterisks\*
	\#hash mark\#
	\[squre brackets\]
### Demonstration 실행결과
\*literal asterisks\*<br>
\#hash mark\#<br>
\[squre brackets\]<br>

## 6. Images
* `<img>`로 변환
* 링크와 비슷하지만 앞에 `!`가 붙음
* 인라인 이미지 \![alt text](/test.png\)
* 링크 이미지 \![alt text](image_URL\)
* 이미지의 사이즈를 변경하기 위해서는 `<img width="OOOpx" height="OOOpx"></img>`와 같이 표현

### Syntax 마크다운 사용법
	![alt 토마토](/img/tomato.jpg)
  	![alt man](/img/man_laptop.jpg)
	![alt Concrete Buildings](https://github.com/sigmadream/practice-markdown/blob/main/img/linux.jpg)

### 실행결과
![alt 윈도우](/img/windows.jpg)
![alt macOS](/img/macos.jpg)
![alt 리눅스](https://github.com/sigmadream/practice-markdown/blob/main/img/linux.jpg)

## 7. Links (Anchor)

## 7.1. External Links
	인라인 링크: [링크](http://example.com "링크 제목")
	url 링크: <example.com>, <example@example.com>; 꺽쇠 괄호 없어도 자동으로 링크를 사용

### Syntax 마크다운 사용법
	[Google](http://www.google.com "구글")
	[Naver](http://www.naver.com "네이버")
	[Github](http://www.github.com "깃허브")
	구글 www.google.com; 꺽쇠없음
	네이버 <www.naver.com>; 꺽쇠있음
	My mail <jinkyukim.dev@gmail.com>

### 실행결과
[Google](http://www.google.com "구글")<br>
[Naver](http://www.naver.com "네이버")<br>
[Github](http://www.github.com "깃허브")<br>
구글 www.google.com <br>
네이버 <www.naver.com> <br>
My mail <jinkyukim.dev@gmail.com><br>

## 7.2. Internal Links 
	[보여지는 내용](#이동할 헤드(제목))
	괄호 안의 링크를 쓸 때는 띄어쓰기는 -로 연결, 영어는 모두 소문자로 작성

### Syntax 마크다운 사용법
	[1. Headers 헤더](#1-headers-헤더)
	[2. Emphasis 강조](#2-emphasis-강조)
	[3. Blockquotes 인용](#3-blockquotes-인용)

### 실행결과
[1. Headers 헤더](#1-headers-헤더)<br>
[2. Emphasis 강조](#2-emphasis-강조)<br>
[3. Blockquotes 인용](#3-blockquotes-인용)<br>

## 8. Fenced Code Blocks
* 간단한 인라인 코드는 텍스트를 앞뒤로 \`기호로 감싸면 됨
* \`\`\` 혹은 ~~~ 코드.
* 첫 줄과 마지막 줄에 Back quote ( \` ) 또는 물결( ~ ) 3개 삽입
* 코드가 여러 줄인 경우, 줄 앞에 공백 네 칸을 추가
* \`\`\` 옆에 언어를 지정해주면 syntax color가 적용

### Syntax 마크다운 사용법
	```
	This is code blocks.
	```
	~~~
	This is code blocks.
	~~~
		4 spaces
	```javascript
	function test() {
	 console.log("look ma’, no spaces");
	}
	```
### Demonstration 실행결과
```
This is code blocks.
```
~~~
This is code blocks.
~~~
	4 spaces
```javascript
function test() {
 console.log("look ma’, no spaces");
}
```

## 9. Task Lisk
* 줄 앞에 `- [x]`를 써서 완료된 리스트 표시.
* 줄 앞에 `- [ ]`를 써서 미완료된 리스트 표시.
* 체크 안에서 강조 외에 여러 기능을 사용할 수 있습니다.

### Syntax 마크다운 사용법
	- [x] this is a complete item
	- [ ] this is an incomplete item
	- [x] @mentions, #refs, [links](),
	**formatting**, and <del>tags</del>
	supported
	- [x] list syntax required (any
	unordered or ordered list
	supported)

###  실행결과
- [x] this is a complete item
- [ ] this is an incomplete item
- [x] @mentions, #refs, [links](),
**formatting**, and <del>tags</del>
supported
- [x] list syntax required (any
unordered or ordered list
supported)

## 10. Horizontal Rules
* \- 또는 * 또는 _ 을 3개 이상 작성.
* 단, -을 사용할 경우 header로 인식할 수 있으니 이 전 라인은 비워두어야 함

### Syntax 마크다운 사용법
	* * *
	***
	*****
	- - -
	-------------------

###  실행결과
* * *
***
*****
- - -
-------------------

## 11. Emoji
* 마크다운을 이용해 이모티콘을 표현가능
* 깃허브도 적용가능
* 더 많은 리스트는 아래의 사이트로 방문
* www.emoji-cheat-sheet.com

### Syntax 마크다운 사용법
	GitHub supports emoji!
	:+1: :sparkles: :camel: :tada:
	:rocket: :metal: :octocat:

### 실행결과
GitHub supports emoji!
:+1: :sparkles: :camel: :tada:
:rocket: :metal: :octocat:

## 12. Table
* 헤더와 셀을 구분할 때 3개 이상의 `-`(hyphen/dash) 기호가 필요
* 헤더 셀을 구분하면서 :(Colons) 기호로 셀(열/칸) 안에 내용을 정렬할 수 있음
* 가장 좌측과 가장 우측에 있는 |(vertical bar) 기호는 생략 가능

### Syntax 마크다운 사용법
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

### 실행결과
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

## 13. Line Breaks
* `<br>`를 활용해서 줄바꿈을 할 수 있음

### Syntax 마크다운 사용법
	言葉はまるで雪の結晶
	말은 마치 눈의 결정 <br>
	君にプレゼントしたくても
	너에게 선물하고 싶어도 <br>

### Demonstration 실행결과
言葉はまるで雪の結晶
말은 마치 눈의 결정 <br>
君にプレゼントしたくても
너에게 선물하고 싶어도 <br>
