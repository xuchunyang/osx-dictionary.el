# OSX *Dictionary.app* interface for Emacs
---

## Introduction
`osx-dictionary.el` is inspired by [dictionary.vim](https://github.com/itchyny/dictionary.vim). `osx-dictionary.el` provides functions which invoke *Dictionary.app* from Mac OS X.

_Notes_: translation between Chinese and English is well supported.

## Prerequisite
* [结巴中文分词](https://github.com/fxsjy/jieba) for Chinese word segmentation (optional)

GNU Emacs doesn't know much about Chinese, for example, it can't not *guess* most likely meaningful Chinese word under current cursor. [结巴中文分词](https://github.com/fxsjy/jieba) is used for this task.

If you don't want to use [结巴中文分词](https://github.com/fxsjy/jieba), just customize `osx-dictionary-chinese-wordsplit-command` to `""`.

## Installation

1. Clone this repository
   ```sh
   $ git clone https://github.com/xuchunyang/osx-dictionary.el
   $ cd osx-dictionary.el
   ```

2. Put `osx-dictionary.el/` to your `load-path` and add the following to
   your Emacs initialization file

   ```emacs-lisp
   (require osx-'dictionary)
   ;; Example key binding
   (global-set-key (kbd "C-c d") 'osx-dictionary-search-pointer)
   (global-set-key (kbd "C-c c") 'osx-dictionary-search-word)
   ```

## Usage
Below are commands you can use:

* `osx-dictionary-search-word` Search word from input
* `osx-dictionary-search-pointer`Search word at point

Here is a screenshot of a sample usage:
![Imgur](http://i.imgur.com/BBg8ZHR.png)

## Todo
- [ ] Save search history
- [ ] Improve regex for highlight
  - [ ] Add more keywords, for example, "名" and "代"
  - [ ] Adopt more precise matching
- [x] Use Chinese text segmentation for better Chinese translation
