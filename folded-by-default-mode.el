;;; folded-by-default-mode --- Minor mode in which files open with function bodies folded.
;;;
;;; Commentary:
;;;
;;; See @matklad's post about the missing IDE feature[1] -- where new file-visiting buffers
;;; open with function bodies initially folded, for a better overview of the code .
;;;
;;; If `ts-fold'[2] is present, this is a thin wrapper around it.  Otherwise, wraps `outline',
;;; `hideshow' or whatever features are present.
;;;
;;; 1: https://matklad.github.io/2024/10/14/missing-ide-feature.html
;;; 2: https://github.com/emacs-tree-sitter/ts-fold/
;;;
;;; Code:


(defgroup folded-by-default-mode nil
  "Folding code by default."
  :group 'tree-sitter
  :prefix "folded-by-default-mode-")

(defcustom folded-by-default-mode-debug
  nil
  "Emit debug messages?"
  :type 'boolean
  :group 'folded-by-default-mode)

(defun folded-by-default-mode--ts-debug (node)
  "Helper: print out debugging information about NODE."
  (message "f-b-d-m: type %S, parent-types %s, alist-p %S, func %S"
           (tsc-node-type node)
           (folded-by-default-mode--ts-node-parent-types node 3)
           (not (not (alist-get major-mode ts-fold-range-alist)))
           (alist-get (tsc-node-type node) (alist-get major-mode ts-fold-range-alist))))

(defun folded-by-default-mode--doom-fold-func (beg end &optional ts-node)
  "Fold regions (from BEG to END, or encompassing TS-NODE) the Doom way.

This is essentially a copy of `+fold/close'."
  (cond ((featurep 'vimish-fold)
         (vimish-fold beg end))
        ((+fold--ensure-hideshow-mode)
         (folded-by-default-mode--hs-fold-func beg end ts-node))
        ((or (bound-and-true-p outline-minor-mode)
             (derived-mode-p 'outline-mode))
         (folded-by-default-mode--outline-fold-func beg end ts-node))
        ((and (bound-and-true-p tree-sitter-mode)
              (featurep 'ts-fold))
         (folded-by-default-mode--ts-fold-func beg end ts-node))))

(defun folded-by-default-mode--ts-fold-func (beg end &optional ts-node)
  "Fold regions (from BEG to END, or encompassing TS-NODE) using `tree-sitter'."
  (ts-fold-close (or ts-node (ts-fold--foldable-node-at-pos (+ beg 1)))))

(defun folded-by-default-mode--outline-fold-func (beg end &optional ts-node)
  "Fold regions (from BEG to END) with `outline-mode'.  Ignore TS-NODE."
  (save-excursion
    (goto-char beg)
    (outline-flag-region (pos-eol) (- end 1) t)))

(defun folded-by-default-mode--hs-fold-func (beg end &optional ts-node)
  "Fold regions (from BEG to END) with `hs-minor-mode'.  Ignore TS-NODE."
  (save-excursion
    (goto-char beg)
    (hs-make-overlay (pos-eol) (- end 1) 'code)))

(defvar folded-by-default-mode--fold-func
  (cond ((and (featurep 'doom)
              (modulep! :editor fold))
         #'folded-by-default-mode--doom-fold-func)
        ((and (bound-and-true-p tree-sitter-mode)
              (featurep 'ts-fold))
         #'folded-by-default-mode--ts-fold-func)
        ((featurep 'outline)
         #'folded-by-default-mode--outline-fold-func)
        ((featurep 'hideshow)
         #'folded-by-default-mode--hs-fold-func)
        (t (error "Cannot define folding by default -- supported folding functions not present!")))
  "Function to run to actually close regions.")

(defun folded-by-default-mode--ts-node-parent-types (node &optional height)
  "What type(s) are the parent of NODE (up to HEIGHT)? Pass DEBUG for messaging."
  (let ((count (or height 1))
        (parents '())
        (node (tsc-get-parent node)))
    (while (and node (> count 0))
      (push (tsc-node-type node) parents)
      (setq node (tsc-get-parent node))
      (setq count (- count 1)))
    (if (length= parents 1)
        (car parents)
      parents)))

(defun folded-by-default-mode--ts-should-fold? (node)
  "Should NODE be folded by default?"
  (when-let* ((type (tsc-node-type node))
              (fold-alist (alist-get major-mode ts-fold-range-alist))
              (fold-func (alist-get (tsc-node-type node) fold-alist)))
    (and (not (eql type 'comment))
         (cond
          ;; Some types should just be folded by default, without further checks
          ((memq type '(block function_definition function_declaration function_item method constructor_declaration constructor_body))
           t)
          ;; C(++): function bodies are compound statements that are immediate children of a function definition
          ((and (eql type 'compound_statement)
                (eql (folded-by-default-mode--ts-node-parent-types node) 'function_definition))
           t)
          ;; JS/Typescript: functions are statement blocks that are immediate children of a function declaration
          ((and (eql type 'statement_block)
                (memq (folded-by-default-mode--ts-node-parent-types node) '(method_definition function_declaration)))
           t)
          ;; JS/Typescript: functions are commonly assigned to variables
          ((and (eql type 'statement_block)
                (let ((parent-types (folded-by-default-mode--ts-node-parent-types node 2)))
                  (eql (nth 1 parent-types) 'function_expression)
                  (memq (nth 0 parent-types) '(assignment_expression variable_declarator))))
           t)
          ;; TODO: block of comments immediately preceding a function definition should also be folded, I would think, as
          ;; they usually are doc comments *for* the function
          ;; Default: don't fold
          (t
           (if folded-by-default-mode-debug (folded-by-default-mode--ts-debug node))
           nil)))))

(defun folded-by-default-mode--ts-do-fold ()
  "Using `tree-sitter' from MELPA, fold all nodes that should be folded by default."
  (iter-do (node (tsc-traverse-iter tree-sitter-tree))
    (when (and (tsc-node-named-p node) (folded-by-default-mode--ts-should-fold? node))
      ;; TODO: The `emacs-tree-sitter' docs point out that using nodes directly
      ;; puts lots of pressure on the garbage collector. Can we mitigate this
      ;; by setting `gc-cons-threshold' before & after this iteration?
      (ts-fold-close node))))

(defun folded-by-default-mode--outline-do-fold ()
  "Using Emacs' built-in `outline', fold subtrees that should be folded by default."
  (error "TODO: Implement outline folding"))

(defun folded-by-default-mode--hideshow-do-fold ()
  "Using Emacs' built-in `hideshow', fold blocks that should be folded by default."
  (error "TODO: Implement hideshow folding"))

(defun folded-by-default-mode--ts-hookify ()
  "Add the hook to `tree-sitter-mode'."
  (add-hook 'tree-sitter-after-first-parse-hook #'folded-by-default-mode--ts-do-fold))

(defun folded-by-default-mode--ts-activate ()
  "Register hooks, etc for `folded-by-default-mode'."
  (require 'ts-fold)
  (add-hook 'global-ts-fold-mode-hook #'folded-by-default-mode--ts-hookify)
  (global-ts-fold-mode 1))

(defun folded-by-default-mode--ts-deactivate ()
  "Register hooks, etc for `folded-by-default-mode'."
  (remove-hook 'tree-sitter-after-first-parse-hook #'folded-by-default-mode--ts-do-fold)
  (remove-hook 'global-ts-fold-mode-hook #'folded-by-default-mode--ts-hookify))

;;;###autoload
(define-minor-mode folded-by-default-mode
  "If enabled, new files open with function / method bodies folded by default."
  :init-value nil
  :interactive nil
  :lighter "\uf003"
  (cond ((fboundp 'ts-fold-mode)
         (require 'tree-sitter)
         (tree-sitter--handle-dependent folded-by-default-mode
           #'folded-by-default-mode--ts-activate
           #'folded-by-default-mode--ts-deactivate))
        ;; TODO: Implement hideshow supporting functionality
        ;; ((fboundp 'hs-minor-mode)
        ;;  (if folded-by-default-mode
        ;;      (folded-by-default-mode--hideshow-activate)
        ;;    (folded-by-default-mode--hideshow-activate)))
        (t (error "Cannot activate folded-by-default-mode, dependencies not present"))))

(define-globalized-minor-mode
  global-folded-by-default-mode
  folded-by-default-mode
  folded-by-default-mode
  :predicate '(prog-mode))

(provide 'folded-by-default-mode)
;;; folded-by-default-mode.el ends here
