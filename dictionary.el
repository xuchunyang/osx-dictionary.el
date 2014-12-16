;;; dictionary.el --- Interface for Dictionary.app

;; Copyright (C) 2014 by Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://github.com/xuchunyang/dictionary.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Example usage:

;;; Code:

(setq dictionary-mode-font-lock-Keywords
      '(;; Word class
        ("noun\\|adjective\\|det\\|verb\\|adverb\\|abbreviation\\|preposition\\|suffix\\|prefix\\|conjunction\\|symb" . font-lock-type-face)
        ;; Serial number
        ("^[0-9]+" . font-lock-builtin-face)
        ;; Dictionary comment
        ("DERIVATIVES\\|ORIGIN\\|PHRASES" . font-lock-comment-face)))

(setq dictionary-mode-map
      (let ((map (make-sparse-keymap)))
        ;; Dictionary command
        (define-key map "q" 'dictionary-quit)
        (define-key map "i" 'dictionary-search-word)
        ;; Isearch
        (define-key map "S" 'isearch-forward-regexp)
        (define-key map "R" 'isearch-backward-regexp)
        (define-key map "s" 'isearch-forward)
        (define-key map "r" 'isearch-backward)
        ;; Misc.
        (define-key map "DEL" 'scroll-down)
        (define-key map " " 'scroll-up)
        (define-key map "l" 'forward-char)
        (define-key map "h" 'backward-char)
        (define-key map "?" 'describe-mode)
        map))

(defconst dictionary-buffer-name "*Dictionary*")

(define-derived-mode dictionary-mode fundamental-mode
  (setq font-lock-defaults '(dictionary-mode-font-lock-Keywords))
  (setq buffer-read-only t)
  (message "dictionary-mode: init"))

(defun dictionary-quit ()
  (interactive)
  (bury-buffer))

(defun dictionary-get-buffer ()
  "Get the dictionary buffer.  Create one if there's none."
  (let ((buffer (get-buffer-create dictionary-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'dictionary-mode)
        (dictionary-mode)))
    buffer))

(defun dictionary-goto-dictionary ()
  "Switch to dictionary buffer in other window."
  (setq dictionary-previous-window-configuration (current-window-configuration))
  (let* ((buffer (dictionary-get-buffer))
         (window (get-buffer-window buffer)))
    (if (null window)
        (switch-to-buffer-other-window buffer)
      (select-window window))))

(defun dictionary-search (word)
  "Search some WORD."
  (shell-command-to-string (format "dictionary %s" word)))

(defun dictionary-search-word ()
  "Prompt for input WORD.
And show translation in other buffer."
  (interactive)
  (let ((word (read-from-minibuffer "Enter word: ")))
    (with-current-buffer (get-buffer-create dictionary-buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (dictionary-search word))
      (dictionary-goto-dictionary)
      (goto-char (point-min))
      (setq buffer-read-only t))))

;;; dictionary.el ends here
