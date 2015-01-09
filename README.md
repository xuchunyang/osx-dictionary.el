# OSX *Dictionary.app* interface for Emacs
---

## Introduction
`osx-dictionary` is inspired by [dictionary.vim](https://github.com/itchyny/dictionary.vim). `osx-dictionary` provides functions which invoke *Dictionary.app* from Mac OS X.

_Notes_: translation between Chinese and English is well supported.

## Prerequisite
* [结巴中文分词](https://github.com/fxsjy/jieba) for Chinese word segmentation (optional)

GNU Emacs doesn't know much about Chinese, for example, it can't not *guess* most likely meaningful Chinese word under current cursor. [结巴中文分词](https://github.com/fxsjy/jieba) is used for this task.

If you don't want to use [结巴中文分词](https://github.com/fxsjy/jieba), just customize `osx-dictionary-chinese-wordsplit-command` to `""`.

## Install
`osx-dictionary` is available on Melpa, and that's the recommended way of
installing it, i.e.

`M-x package-install RET osx-dictionary RET`

## Usage
Below are commands you can use:

* `osx-dictionary-search-input` Search input word and display result with buffer
* `osx-dictionary-search-pointer` Search word around and display result with buffer

Here is a screenshot of a sample usage:
![Imgur](http://i.imgur.com/BBg8ZHR.png)

## Todo
- [ ] Save search history
- [ ] Improve regex for highlight
  - [ ] Add more keywords, for example, "名" and "代"
  - [ ] Adopt more precise matching
- [x] Use Chinese text segmentation for better Chinese translation
