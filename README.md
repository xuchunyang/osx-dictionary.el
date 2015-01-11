# OSX *Dictionary.app* interface for Emacs
---
[![MELPA](http://melpa.org/packages/osx-dictionary-badge.svg)](http://melpa.org/#/osx-dictionary)

## Introduction
`osx-dictionary` is inspired by [dictionary.vim](https://github.com/itchyny/dictionary.vim). `osx-dictionary` provides functions which invoke *Dictionary.app* from Mac OS X.

_Notes_: translation between Chinese and English is well supported.

## Requirement

* [chinese-word-at-point](https://github.com/xuchunyang/chinese-word-at-point.el) to get (most likely) Chinese word under the cursor

If you don't use Chinese, no needs to care about it, features provided by
[chinese-word-at-point](https://github.com/xuchunyang/chinese-word-at-point.el)
is disabled by default.

## Install

`osx-dictionary` is available on Melpa, and that's the recommended way of
installing it, i.e.

`M-x package-install RET osx-dictionary RET`

In this way, the requirement,
[chinese-word-at-point](https://github.com/xuchunyang/chinese-word-at-point.el),
will be installed automatically.

## Usage
Below are commands you can use:

* `osx-dictionary-search-input` Search input word and display result with buffer
* `osx-dictionary-search-pointer` Search word around and display result with buffer

GNU Emacs itself has no idea what a *Chinese word* is, If you want Emacs to get
*Chinese word* under the cursor, you have to set
`osx-dictionary-use-chinese-text-segmentation` to `t` by yourself. Please refer
to
[chinese-word-at-point](https://github.com/xuchunyang/chinese-word-at-point.el)
for more info.

Below is my configuration for this package:
```elisp
(setq osx-dictionary-use-chinese-text-segmentation t) ; Support Chinese word
(global-set-key (kbd "C-c d") 'osx-dictionary-search-pointer)
;; (global-set-key (kbd "C-c i") 'osx-dictionary-search-input)
```

Here is a screenshot of a sample usage:
![Imgur](http://i.imgur.com/BBg8ZHR.png)

## Todo
- [ ] Save search history
- [ ] Improve regex for highlight
  - [ ] Add more keywords, for example, "名" and "代"
  - [ ] Adopt more precise matching
- [x] Use Chinese text segmentation for better Chinese translation
