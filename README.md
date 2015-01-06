# OSX *Dictionary.app* interface for Emacs
---

## Introduction
`osx-dictionary.el` is Emacs port of [dictionary.vim](https://github.com/itchyny/dictionary.vim). `osx-dictionary.el` provides functions which invoke Mac OS X's *Dictionary.app*.

_Notes_: currently, only translation between Chinese and English is well supported.

## Installation

1. Compile & install `dictionary`

   `osx-dictionary` is a simple CommandLine utility to access *Dictionary.app*

   ```sh
   clang -O3 -framework CoreServices -framework Foundation osx-dictionary.m -o osx-dictionary
   sudo mv osx-dictionary /usr/local/bin
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

* `osx-dictionary-search-word` Search word from input via minibuffer
* `osx-dictionary-search-pointer`Search word under pointer

Here is a screenshot of a sample usage:
![Imgur](http://i.imgur.com/BBg8ZHR.png)

## Todo
- [ ] Save search history
- [ ] Improve regex for highlight
  - [ ] Add more keywords, for example, "名" and "代"
  - [ ] Adopt more precise matching
- [ ] Use Chinese text segmentation for better Chinese translation
