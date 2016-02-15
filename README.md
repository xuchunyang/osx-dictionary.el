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

* `osx-dictionary-search-pointer` Search word around and display result with buffer
* `osx-dictionary-search-input` Search input word and display result with buffer

## Sample configuration

```elisp
;; Support Chinese word
;; (setq osx-dictionary-use-chinese-text-segmentation t)

;; Choose explicitly a dictionary for searching (use the first available
;; dictionary in Dictionary.app if not set)
;; (setq osx-dictionary-dictionary-choice "Apple")
;; To search in more than one dictionaries
;; (setq osx-dictionary-dictionary-choice (list "English" "Simplified Chinese" "Spanish"))
;; To search in all dictionaries
;; (setq osx-dictionary-dictionary-choice (osx-dictionary-get-all-dictionaries))

;; Key bindings
(global-set-key (kbd "C-c d") 'osx-dictionary-search-pointer)
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

## Todo
- [x] Save search history
- [ ] Improve regex for highlight
  - [ ] Add more keywords, for example, "名" and "代"
  - [ ] Adopt more precise matching
- [x] Use Chinese text segmentation for better Chinese translation
