# OSX *Dictionary.app* interface for Emacs
---
[![MELPA](http://melpa.org/packages/osx-dictionary-badge.svg)](http://melpa.org/#/osx-dictionary)
[![MELPA](http://stable.melpa.org/packages/osx-dictionary-badge.svg)](http://stable.melpa.org/#/osx-dictionary)

## Introduction

`osx-dictionary` is inspired by [dictionary.vim](https://github.com/itchyny/dictionary.vim). `osx-dictionary` provides functions which invoke *Dictionary.app* from Mac OS X.

## Installation

Install from [MELPA](http://melpa.org) with:

    M-x package-install RET osx-dictionary RET

## Usage

* `osx-dictionary-search-word-at-point` Search word at point and display result with buffer
* `osx-dictionary-search-input` Search input word and display result with buffer

## Sample configuration

```elisp
;; Support Chinese word
;; (setq osx-dictionary-use-chinese-text-segmentation t)

;; Key bindings
(global-set-key (kbd "C-c d") 'osx-dictionary-search-word-at-point)
;; (global-set-key (kbd "C-c i") 'osx-dictionary-search-input)

;; Work with popwin-el (https://github.com/m2ym/popwin-el)
;; (push "*osx-dictionary*" popwin:special-display-config)
```

Here is a screenshot of a sample usage:
![Imgur](http://i.imgur.com/BBg8ZHR.png)

## Note on Chinese word support

GNU Emacs itself has no idea what a *Chinese word* is, If you want Emacs to get
the most likely *Chinese word* under the cursor, you have to set
`osx-dictionary-use-chinese-text-segmentation` to `t` and install a
Chinese word segmentation tool, please refer to
[chinese-word-at-point#prerequisite](https://github.com/xuchunyang/chinese-word-at-point.el#prerequisite)
for more info.

## News

- 2016/11 The option `osx-dictionary-dictionary-choice` was removed AFTER v0.2.2 for macOS Sierra support.
