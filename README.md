# Dictionary.app interface for Emacs

## Introduction
`dictionary.el` is Emacs port of [dictionary.vim](https://github.com/itchyny/dictionary.vim). `dictionary.el` provides functions which invoke Mac OS X's *Dictionary.app*.

## Installation

1. Compile & install `dictionary`

   `dictionary` is a simple CommandLine utility to access *Dictionary.app*

   ```sh
   clang -O3 -framework CoreServices -framework Foundation dictionary.m -o dictionary
   sudo mv dictionary /usr/local/bin
   ```

2. Put `dictionary.el` to your `load-path` and add the following to
   your Emacs initialization file

   ```emacs-lisp
   (require 'dictionary)
   ;; Example key binding
   (global-set-key (kbd "C-c d") 'dictionary-search-pointer)
   ```

## Usage

```emacs-lisp
;; Below are commands you can use:
;; `dictionary-search-word'
;; Search word from input via minibuffer
;; `dictionary-search-pointer'
;; Search word under pointer (cursor)
```
