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
;; `osx-dictionary-select-dictionary'
;; Restrict lookups to one installed dictionary (or back to all active ones);
;; the choice persists across sessions, and the offered dictionaries -- and
;; their order in the prompt -- can be narrowed with
;; `osx-dictionary-allowed-dictionaries'
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

(defcustom osx-dictionary-last-dictionary-file
  (locate-user-emacs-file "osx-dictionary-last-dictionary")
  "File used to persist the dictionary chosen by `osx-dictionary-select-dictionary'.
Set to nil to disable persistence, so the choice only lasts the session."
  :type '(choice (const :tag "Don't persist across sessions" nil)
                 (file :tag "File"))
  :group 'osx-dictionary)

(defcustom osx-dictionary-allowed-dictionaries nil
  "Dictionaries `osx-dictionary-select-dictionary' offers, and their order.
Nil offers every dictionary installed in Dictionary.app (the default), in
whatever order it reports them.  Otherwise a list of dictionary names,
offered in that order; a name no longer installed is silently dropped."
  :type '(choice (const :tag "Offer every installed dictionary" nil)
                 (repeat :tag "Names, in this order" string))
  :group 'osx-dictionary)

(defvar osx-dictionary-current-dictionary nil
  "Name of the dictionary `osx-dictionary' restricts lookups to.
Nil means search every dictionary enabled in Dictionary.app (the default).
Set via `osx-dictionary-select-dictionary', which also persists it to
`osx-dictionary-last-dictionary-file'.")

(defface osx-dictionary-dictionary-name
  '((t :inherit (org-level-1 bold)))
  "Face for the heading naming the current dictionary restriction.
Falls back to `bold' if `org-level-1' is not defined (Org not loaded)."
  :group 'osx-dictionary)

(defun osx-dictionary--current-dictionary-description ()
  "Human-readable description of what `osx-dictionary--search' currently restricts to."
  (or osx-dictionary-current-dictionary
      (if osx-dictionary-allowed-dictionaries
          "All allowed dictionaries"
        "All active dictionaries")))

(defun osx-dictionary--load-last-dictionary ()
  "Restore `osx-dictionary-current-dictionary' from `osx-dictionary-last-dictionary-file'."
  (when (and osx-dictionary-last-dictionary-file
             (file-exists-p osx-dictionary-last-dictionary-file))
    (setq osx-dictionary-current-dictionary
          (with-temp-buffer
            (insert-file-contents osx-dictionary-last-dictionary-file)
            (ignore-errors (read (current-buffer)))))))

(defun osx-dictionary--save-last-dictionary ()
  "Persist `osx-dictionary-current-dictionary' to `osx-dictionary-last-dictionary-file'."
  (when osx-dictionary-last-dictionary-file
    (with-temp-file osx-dictionary-last-dictionary-file
      (prin1 osx-dictionary-current-dictionary (current-buffer)))))

(osx-dictionary--load-last-dictionary)

(defconst osx-dictionary-cli "osx-dictionary-cli"
  "The name of executable file compiled from \"osx-dictionary.m\".")

(defvar osx-dictionary-buffer-name "*osx-dictionary*")

(defun osx-dictionary-generate-buffer-name-default-function (_word)
  osx-dictionary-buffer-name)

(defcustom osx-dictionary-generate-buffer-name-function
  'osx-dictionary-generate-buffer-name-default-function
  "The function used to generate the name for a osx-dictionary buffer.
The function takes the WORD as the sole argument."
  :group 'osx-dictionary
  :type '(radio (function-item osx-dictionary-generate-buffer-name-default-function)
                (function :tag "Function")))

(defconst osx-dictionary--load-dir (file-name-directory
                                    (or load-file-name buffer-file-name)))

(defvar osx-dictionary-mode-header-line
  '(
    (:propertize "s" face mode-line-buffer-id)
    ": Search Word"
    "    "
    (:propertize "S" face mode-line-buffer-id)
    ": Choose Dictionary"
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
    (define-key map "S" 'osx-dictionary-select-dictionary)
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
  (shell-command (format "open dict://%s" (osx-dictionary--get-current-word))))

(defun osx-dictionary-read-word ()
  "Read current searched `word' using text-to-speech service."
  (interactive)
  (shell-command (concat "say " (shell-quote-argument (osx-dictionary--get-current-word)))))

(defvar-local osx-dictionary--current-word nil
  "The word displayed in this `osx-dictionary-mode' buffer.
Set by `osx-dictionary--view-result'.")

(defun osx-dictionary--get-current-word ()
  "Return the word displayed in the current `osx-dictionary-mode' buffer.
Falls back to scanning from the top of the buffer if
`osx-dictionary--current-word' was never set -- do not rely on this
fallback, since a labeled result's first line is now a dictionary-name
heading, not the word."
  (or osx-dictionary--current-word
      (save-excursion
        (goto-char (point-min))
        (replace-regexp-in-string (rx "·") "" (current-word)))))

(defun osx-dictionary-quit ()
  "Quit osx-dictionary: reselect previously selected buffer."
  (interactive)
  (if (window-configuration-p osx-dictionary-previous-window-configuration)
      (progn
        (set-window-configuration osx-dictionary-previous-window-configuration)
        (setq osx-dictionary-previous-window-configuration nil)
        (bury-buffer))
    (bury-buffer)))

(defun osx-dictionary--get-buffer (word)
  "Get the osx-dictionary buffer.  Create one if there's none."
  (let ((buffer (get-buffer-create (funcall osx-dictionary-generate-buffer-name-function word))))
    (with-current-buffer buffer
      (unless (eq major-mode 'osx-dictionary-mode)
        (osx-dictionary-mode)))
    buffer))

(defun osx-dictionary--goto-dictionary (word)
  "Switch to osx-dictionary buffer in other window."
  (setq osx-dictionary-previous-window-configuration
        (current-window-configuration))
  (let* ((buffer (osx-dictionary--get-buffer word))
         (window (get-buffer-window buffer)))
    (if (null window)
        (switch-to-buffer-other-window buffer)
      (select-window window))))

(defun osx-dictionary--search-dictionary-args ()
  "Return the \"-d NAME ...\" args restricting a search, or nil for none.
One `-d' per name in `osx-dictionary-current-dictionary' when set; otherwise
one per name in `osx-dictionary-allowed-dictionaries', when that is set;
nil when neither applies, so the CLI falls back to Dictionary.app's own
\"all active dictionaries\" search."
  (let ((names (cond (osx-dictionary-current-dictionary
                      (list osx-dictionary-current-dictionary))
                     (osx-dictionary-allowed-dictionaries
                      (osx-dictionary--selectable-dictionaries)))))
    (when names
      (mapconcat (lambda (name) (concat "-d " (shell-quote-argument name)))
                 names " "))))

(defun osx-dictionary--search (word)
  "Search WORD, restricted per `osx-dictionary--search-dictionary-args'.
When more than one dictionary is queried, each one's block is preceded by
a line of the form \\x01NAME\\x01 -- see `osx-dictionary--insert-search-result'."
  ;; Save to history file
  (when osx-dictionary-search-log-file
    (append-to-file
     (concat word "\n") nil
     (expand-file-name osx-dictionary-search-log-file)))
  ;; Search
  (shell-command-to-string
   (format "%s %s%s 2>/dev/null"
           (shell-quote-argument (osx-dictionary-cli-find-or-recompile))
           (let ((args (osx-dictionary--search-dictionary-args)))
             (if args (concat args " ") ""))
           (shell-quote-argument word))))

(defconst osx-dictionary--bullet-indent "  "
  "Hanging indent given to bullet-marked sub-senses (▸/• lines).
Applied via the `line-prefix'/`wrap-prefix' text properties -- see
`osx-dictionary--indent-bullets' -- rather than literal characters, so a
bullet line that wraps across multiple screen lines keeps the indent on
every continuation, not just the first.")

(defun osx-dictionary--indent-bullets (start end)
  "Give each bullet-marked line (▸/•) between START and END a hanging indent.
`osx-dictionary.m' forces such a line onto its own line but no longer pads
it with literal spaces, leaving the actual indent to Emacs."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "^[▸•] " end t)
      (let ((bol (line-beginning-position))
            (eol (line-end-position)))
        (put-text-property bol eol 'line-prefix osx-dictionary--bullet-indent)
        (put-text-property bol eol 'wrap-prefix osx-dictionary--bullet-indent)))))

(defun osx-dictionary--insert-search-result (word)
  "Insert the search result for WORD at point.
Turns each \\x01NAME\\x01 marker line `osx-dictionary--search' may have
embedded (one per dictionary that was queried) into a heading naming that
dictionary, styled with `osx-dictionary-dictionary-name'; and gives each
bullet-marked line a hanging indent, see `osx-dictionary--indent-bullets'."
  (let ((start (point)))
    (insert (osx-dictionary--search word))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\x01\\([^\x01\n]*\\)\x01\n" nil t)
        (replace-match
         (concat (propertize (match-string 1) 'font-lock-face 'osx-dictionary-dictionary-name)
                 "\n")
         nil t)))
    (osx-dictionary--indent-bullets start (point))))

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
      (with-current-buffer (get-buffer-create
                            (funcall osx-dictionary-generate-buffer-name-function word))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (let ((progress-reporter
                 (make-progress-reporter (format "Searching (%s)..." word)
                                         nil nil)))
            (osx-dictionary--insert-search-result word)
            (progress-reporter-done progress-reporter))
          (osx-dictionary--goto-dictionary word)
          ;; After `osx-dictionary--goto-dictionary', so `osx-dictionary-mode'
          ;; (which kills buffer-local variables) is already on; setting this
          ;; any earlier would be wiped out by it turning on for a fresh buffer.
          (setq osx-dictionary--current-word word)
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

;;;###autoload
(defalias 'osx-dictionary-search-word-at-point 'osx-dictionary-search-pointer)

;;;###autoload
(defun osx-dictionary-get-all-dictionaries ()
  "Get the names of all dictionaries installed in Dictionary.app, as a list."
  (split-string
   (shell-command-to-string
    (format "%s -l" (shell-quote-argument (osx-dictionary-cli-find-or-recompile))))
   "\n" t))

(defun osx-dictionary--selectable-dictionaries ()
  "Names `osx-dictionary-select-dictionary' offers, in display order.
Honors `osx-dictionary-allowed-dictionaries', dropping any name it lists
that is no longer installed."
  (let ((installed (osx-dictionary-get-all-dictionaries)))
    (if osx-dictionary-allowed-dictionaries
        (seq-filter (lambda (name) (member name installed))
                    osx-dictionary-allowed-dictionaries)
      installed)))

;;;###autoload
(defun osx-dictionary-select-dictionary (&optional dictionary)
  "Restrict `osx-dictionary' lookups to DICTIONARY.
Interactively, prompts among the dictionaries installed in Dictionary.app
(narrowed and ordered by `osx-dictionary-allowed-dictionaries' when set),
plus a final \"All ...\" choice to search every one of them (the default,
with no restriction, being every dictionary enabled in Dictionary.app).
Always prompts in that same fixed order: the current selection is not
preselected, so re-picking it takes an explicit choice like any other --
there is little point defaulting to what is already in effect, and doing
so would only make the familiar order unpredictable.  The choice is
persisted to `osx-dictionary-last-dictionary-file' and used by later
lookups, including in future sessions.  Called from within
`osx-dictionary-mode' (e.g. its \"S\" key), also re-searches the word
already displayed, so switching dictionary shows that word's entry there
instead."
  (interactive
   (let* ((all (if osx-dictionary-allowed-dictionaries
                   "All allowed dictionaries"
                 "All active dictionaries"))
          (choice (completing-read
                   "Dictionary: "
                   (append (osx-dictionary--selectable-dictionaries) (list all))
                   nil t)))
     (list (unless (string= choice all) choice))))
  (setq osx-dictionary-current-dictionary dictionary)
  (osx-dictionary--save-last-dictionary)
  (message "osx-dictionary: now searching %s"
           (osx-dictionary--current-dictionary-description))
  (when (derived-mode-p 'osx-dictionary-mode)
    (osx-dictionary--view-result (osx-dictionary--get-current-word))))

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
