;;; inter.el --- Major mode for the Inter language

;;; Commentary:
;; Provides syntax highlighting for the Inter programming language.
;; To use, save this file as "inter.el" in your Emacs load-path,
;; and add (require 'inter) and (add-to-list 'auto-mode-alist '("\\.inter\\'" . inter-mode))
;; to your Emacs configuration file (e.g., ~/.emacs.d/init.el or ~/.emacs).
;; Replace ".inter" with the actual file extension for your language if different.

;;; Code:

(defvar inter-mode-syntax-table nil
  "Syntax table for Inter mode.")

(unless inter-mode-syntax-table
  (setq inter-mode-syntax-table (make-syntax-table))
  ;; C-style comments "//"
  ;; The 'b' flag is for C++-style comments (alternative comment syntax)
  (modify-syntax-entry ?\/ ". 12b" inter-mode-syntax-table) ; First / is punc, second / starts comment
  (modify-syntax-entry ?\n "> b" inter-mode-syntax-table)  ; Newline ends this style of comment
  ;; String literal syntax
  (modify-syntax-entry ?\" "\"" inter-mode-syntax-table) ; " is string delimiter
  ;; Underscore is part of words
  (modify-syntax-entry ?_ "w" inter-mode-syntax-table)
  ;; Other defaults from prog-mode are usually fine
  )

;; This variable holds the list of rules for syntax highlighting.
(defvar inter-font-lock-keywords
  `(
    ;; 1. Comments (e.g., // This is a comment)
    ;; Must be early to ensure comment text isn't matched by other rules.
    ("\\(//.*\\)" . font-lock-comment-face)

    ;; 2. String literals (e.g., "hello world")
    ;; Must be early to correctly parse contents.
    ("\"\\(?:\\\\.\\|[^\"\\\\]\\)*\"" . font-lock-string-face)

    ;; 3. Built-in functions/macros (e.g., #print, #length)
    ;; Specific constructs like these should be matched before more general rules.
    ("\\(#\\(?:print\\|length\\|range\\|println\\|stack\\|sleep\\|array\\)\\)\\b" . font-lock-builtin-face)

    ;; 4. Function names in definition (e.g., my_func :: (...))
    ;; Catches: add :: (x int, y int) int {
    ;; The '1' means only the first captured group (the function name) is highlighted.
    ("\\b\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*::\\s-*(" 1 font-lock-function-name-face)

    ;; 5. Struct names in definition (e.g., MyStruct :: struct)
    ;; Catches: Point2D :: struct {
    ;; Assumes struct names start with an uppercase letter, common convention.
    ("\\b\\([A-Z][a-zA-Z0-9_]*\\)\\s-*::\\s-*struct" 1 font-lock-type-face)

    ;; 6. Keywords
    ;; \b ensures matching whole words.
    ;; Covers: struct, return, let, for, in, if, else, while
    ("\\b\\(struct\\|return\\|let\\|for\\|in\\|if\\|else\\|while\\)\\b" . font-lock-keyword-face)
    ;; The '::' operator is also treated as a keyword-like element.
    ("::" . font-lock-keyword-face)

    ;; 7. Primitive Types and 'unit' type
    ;; Covers: int, unit
    ("\\b\\(int\\|float\\|unit\\|string\\|bool\\|true\\|false\\)\\b" . font-lock-type-face)

    ;; 8. User-defined types (heuristic: capitalized words not caught by other rules)
    ;; This will catch 'Point2D' in 'let p Point2D =' or parameter '(p Point2D)'
    ;; This rule is placed after more specific keyword/type rules to avoid conflicts.
    ("\\b[A-Z][a-zA-Z0-9_]*\\b" . font-lock-type-face)

    ;; 9. Numbers (integers in this example)
    ("\\b[0-9]+\\b" . font-lock-constant-face)
    )
  "Syntax highlighting rules for Inter mode.")

;;;###autoload
(define-derived-mode inter-mode prog-mode "Inter"
  "Major mode for editing Inter language files."
  :syntax-table inter-mode-syntax-table
  (setq-local comment-start "// ") ; Define how comments start for comment commands
  (setq-local comment-start-skip "\\(//+\\s-*\\)") ; Regex to skip existing comment chars
  ;; Set the font-lock defaults for this mode.
  ;; '((inter-font-lock-keywords))' tells font-lock to use the rules
  ;; defined in the variable inter-font-lock-keywords.
  (setq font-lock-defaults '((inter-font-lock-keywords))))

;; Associate .inter files (example extension) with this mode
;; If you don't want this line here, you can add it to your init.el:
;; (add-to-list 'auto-mode-alist '("\\.inter\\'" . inter-mode))

(provide 'inter) ; Corresponds to (require 'inter) for loading the file "inter.el"

;;; inter.el ends here
