;;; osx-dictionary.el --- Interface for OSX Dictionary.app  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2016 by Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
;; Maintainer: Chunyang Xu <mail@xuchunyang.me>
;; URL: https://github.com/xuchunyang/osx-dictionary.el
;; Package-Requires: ((cl-lib "0.5"))
;; keywords: mac, dictionary
;; Version: 0.4

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
;;
;; Interface for OSX Dictionary.app
;;
;; Translation word by Dictionary.app, and display result using buffer.
;;
;; Below are commands you can use:
;; `osx-dictionary-search-input'
;; Search input word and display result with buffer
;; `osx-dictionary-search-word-at-point'
;; Search word around and display result with buffer
;;

;;; Installation:
;;
;; This package is available on Melpa.
;;

;;; Code:
(require 'cl-lib)

(defgroup osx-dictionary nil
  "Mac OS X Dictionary.app interface for Emacs."
  :group 'external)

(defcustom osx-dictionary-use-chinese-text-segmentation nil
  "Set to t to enable Chinese text segmentation.

A external Chinese text segmentation tool is required, refer to
URL `https://github.com/xuchunyang/chinese-word-at-point.el'
for more info."
  :type 'boolean
  :group 'osx-dictionary)

(defcustom osx-dictionary-search-log-file nil
  "File for saving searching history."
  :type '(choice (const :tag "Don't write search history to file" nil)
                 (string :tag "Name of log file"))
  :group 'osx-dictionary)

(defcustom osx-dictionary-separator "--------------------\n"
  "Definitions separator."
  :type 'string
  :group 'osx-dictionary)

(defconst osx-dictionary-cli "osx-dictionary-cli"
  "The name of executable file compiled from \"osx-dictionary.m\".")

(defconst osx-dictionary-buffer-name "*osx-dictionary*")

(defconst osx-dictionary--load-dir (file-name-directory
                                    (or load-file-name buffer-file-name)))

(defvar osx-dictionary-mode-header-line
  '(
    (:propertize "s" face mode-line-buffer-id)
    ": Search Word"
    "    "
    (:propertize "o" face mode-line-buffer-id)
    ": Open in Dictionary.app"
    "    "
    (:propertize "r" face mode-line-buffer-id)
    ": Read word"
    "    "
    (:propertize "q" face mode-line-buffer-id)
    ": Quit")
  "Header-line used on the `osx-dictionary-mode'.")

(defvar osx-dictionary-mode-font-lock-keywords
  '(
    ;; Word class
    ("\\b\\(noun\\|adjective\\|det\\|verb\\|adverb\\|abbreviation\\|preposition\\|suffix\\|prefix\\|conjunction\\|symb\\)\\b" . font-lock-type-face)
    ;; Serial number
    ("^[0-9]+" . font-lock-builtin-face)
    ;; Dictionary comment
    ("^\\(DERIVATIVES\\|ORIGIN\\|PHRASES\\)" . font-lock-comment-face))
  "Keywords to highlight in `osx-dictionary-mode'.")

(defvar osx-dictionary-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Dictionary commands
    (define-key map "q" 'osx-dictionary-quit)
    (define-key map "s" 'osx-dictionary-search-input)
    (define-key map "o" 'osx-dictionary-open-dictionary.app)
    (define-key map "r" 'osx-dictionary-read-word)
    ;; Misc
    (define-key map "?" 'describe-mode)
    map)
  "Keymap for `osx-dictionary-mode'.")

(defvar osx-dictionary-previous-window-configuration nil
  "Window configuration before switching to dictionary buffer.")

(define-derived-mode osx-dictionary-mode fundamental-mode "osx-dictionary"
  "Major mode to look up word through dictionary.
\\{osx-dictionary-mode-map}.
Turning on Text mode runs the normal hook `osx-dictionary-mode-hook'."

  (setq header-line-format osx-dictionary-mode-header-line)
  (setq font-lock-defaults '(osx-dictionary-mode-font-lock-keywords)))

(add-hook 'osx-dictionary-mode-hook #'read-only-mode)
(add-hook 'osx-dictionary-mode-hook #'visual-line-mode)

(defun osx-dictionary-open-dictionary.app ()
  "Open current searched `word' in Dictionary.app."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (shell-command (format "open dict://%s" (thing-at-point 'word)))))

(defun osx-dictionary-read-word ()
  "Open current searched `word' in Dictionary.app."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (shell-command (concat "say " (shell-quote-argument (thing-at-point 'word))))))

(defun osx-dictionary-quit ()
  "Quit osx-dictionary: reselect previously selected buffer."
  (interactive)
  (if (window-configuration-p osx-dictionary-previous-window-configuration)
      (progn
        (set-window-configuration osx-dictionary-previous-window-configuration)
        (setq osx-dictionary-previous-window-configuration nil)
        (bury-buffer (osx-dictionary--get-buffer)))
    (bury-buffer)))

(defun osx-dictionary--get-buffer ()
  "Get the osx-dictionary buffer.  Create one if there's none."
  (let ((buffer (get-buffer-create osx-dictionary-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'osx-dictionary-mode)
        (osx-dictionary-mode)))
    buffer))

(defun osx-dictionary--goto-dictionary ()
  "Switch to osx-dictionary buffer in other window."
  (setq osx-dictionary-previous-window-configuration
        (current-window-configuration))
  (let* ((buffer (osx-dictionary--get-buffer))
         (window (get-buffer-window buffer)))
    (if (null window)
        (switch-to-buffer-other-window buffer)
      (select-window window))))

(defun osx-dictionary--search (word)
  "Search WORD."
  ;; Save to history file
  (when osx-dictionary-search-log-file
    (append-to-file
     (concat word "\n") nil
     (expand-file-name osx-dictionary-search-log-file)))
  ;; Search
  (shell-command-to-string
   (format "%s %s"
           (shell-quote-argument (osx-dictionary-cli-find-or-recompile))
           (shell-quote-argument word))))

(defun osx-dictionary-recompile ()
  "Create or replace the `osx-dictionary-cli' executable using the latest code."
  (interactive)
  (let ((default-directory osx-dictionary--load-dir))
    (shell-command (concat "clang -O3 -framework CoreServices -framework Foundation osx-dictionary.m -o "
                           (shell-quote-argument osx-dictionary-cli)))
    (expand-file-name osx-dictionary-cli)))

(defun osx-dictionary-cli-find-or-recompile ()
  "Find the osx-dictionary-cli.  If it does not exist, recompile it."
  (or
   (executable-find (expand-file-name osx-dictionary-cli osx-dictionary--load-dir))
   (executable-find osx-dictionary-cli)
   (osx-dictionary-recompile)))

(defun osx-dictionary--view-result (word)
  "Make buffer for the searching result of WORD."
  (if word
      (with-current-buffer (get-buffer-create osx-dictionary-buffer-name)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (let ((progress-reporter
                 (make-progress-reporter (format "Searching (%s)..." word)
                                         nil nil)))
            (insert (osx-dictionary--search word))
            (progress-reporter-done progress-reporter))
          (osx-dictionary--goto-dictionary)
          (goto-char (point-min))
          (let ((buffer-read-only nil))
            (whitespace-cleanup))))
    (message "Nothing to look up")))

;;;###autoload
(defun osx-dictionary-search-input ()
  "Search input word and display result with buffer."
  (interactive)
  (let* ((default (osx-dictionary--region-or-word))
         (prompt  (if default (format "Word (%s): " default)
                    "Word: "))
         (word (read-string prompt nil nil default)))
    (osx-dictionary--view-result word)))

;;;###autoload
(defun osx-dictionary-search-pointer ()
  "Search word around and display result with buffer."
  (interactive)
  (let ((word (osx-dictionary--region-or-word)))
    (osx-dictionary--view-result word)))

(defalias 'osx-dictionary-search-word-at-point 'osx-dictionary-search-pointer)

;;;###autoload
(defun osx-dictionary-get-all-dictionaries ()
  "Get all dictionaries as a list."
  (split-string
   (shell-command-to-string
    (format "%s -l" (shell-quote-argument (osx-dictionary-cli-find-or-recompile))))
   "\n"))

(defun osx-dictionary--region-or-word ()
  "Return region or word around point.
If `mark-active' on, return region string.
Otherwise return word around point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (if osx-dictionary-use-chinese-text-segmentation
        (if (require 'chinese-word-at-point nil t)
            (thing-at-point 'chinese-or-other-word)
          (user-error "The package chinese-word-at-point isn't installed"))
      (thing-at-point 'word))))

(provide 'osx-dictionary)

;;; osx-dictionary.el ends here

;; Local Variables:
;; coding: utf-8
;; End:
