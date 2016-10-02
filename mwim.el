;;; mwim.el --- Move to the beginning/end of line or code  -*- lexical-binding: t -*-

;; Copyright © 2015, 2016 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 9 Jan 2015
;; Version: 0.3
;; URL: https://github.com/alezost/mwim.el
;; Keywords: convenience

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

;; MWIM stands for "Move Where I Mean".  This package is inspired by
;; <http://www.emacswiki.org/emacs/BackToIndentationOrBeginning>.  It
;; provides commands for moving to the beginning/end of code, line or
;; comment.

;; To install the package manually, add the following to your init file:
;;
;;   (add-to-list 'load-path "/path/to/mwim-dir")
;;   (autoload 'mwim-beginning "mwim" nil t)
;;   (autoload 'mwim-end "mwim" nil t)
;;   (autoload 'mwim-beginning-of-code-or-line "mwim" nil t)
;;   (autoload 'mwim-beginning-of-line-or-code "mwim" nil t)
;;   (autoload 'mwim-beginning-of-code-or-line-or-comment "mwim" nil t)
;;   (autoload 'mwim-end-of-code-or-line "mwim" nil t)
;;   (autoload 'mwim-end-of-line-or-code "mwim" nil t)

;; Then you can bind some keys to some of those commands and start
;; moving.  See README in the source repo for more details.

;;; Code:

(defgroup mwim nil
  "Move Where I Mean.
Move the point to the beginning/end of code or line."
  :group 'convenience)

(defcustom mwim-beginning-of-line-function #'beginning-of-line
  "Function used to move the point to the beginning of line."
  :type '(choice (function-item beginning-of-visual-line)
                 (function-item beginning-of-line)
                 (function :tag "Another function"))
  :group 'mwim)

(defcustom mwim-end-of-line-function #'end-of-line
  "Function used to move the point to the end of line."
  :type '(choice (function-item end-of-visual-line)
                 (function-item end-of-line)
                 (function :tag "Another function"))
  :group 'mwim)


;;; Calculating positions

(defmacro mwim-point-at (&rest body)
  "Return point position after evaluating BODY in `save-excursion'."
  (declare (debug t) (indent 0))
  `(save-excursion ,@body (point)))

(defun mwim-first-position (expressions &optional position)
  "Return the first point position that is not POSITION from
positions defined after evaluating EXPRESSIONS.

If POSITION is nil, use the current point position.

Initially, the first expression is evaluated.  If the resulting
position is not the same as POSITION, return it.  Otherwise,
evaluate the second expression, etc.

If after evaluating all EXPRESSIONS, all resulting positions are
the same as POSITION, return nil."
  (or position (setq position (point)))
  (pcase expressions
    (`(,first . ,rest)
     (let ((pos (eval first)))
       (if (and pos (/= pos position))
           pos
         (mwim-first-position rest position))))))

(defun mwim-next-position (expressions &optional position fallback-position)
  "Return the next point position after POSITION from positions
defined after evaluating EXPRESSIONS.

If POSITION is nil, use the current point position.

Initially, the first expression is evaluated.  If the resulting
position is the same as POSITION, return position defined after
evaluating the second expression.  If it is not the same, compare
the second position with POSITION, etc.

If after evaluating all EXPRESSIONS, POSITION is not one of the
found positions, return FALLBACK-POSITION.  If it is nil, return
the first position."
  (or position (setq position (point)))
  (if (null expressions)
      fallback-position
    (pcase expressions
      (`(,first . ,rest)
       ;; If the last expression is reached, there is no point to evaluate
       ;; it, as the point should be moved to the first position anyway.
       (if (and (null rest) fallback-position)
           fallback-position
         (let ((pos (eval first)))
           (if (and pos (= pos position))
               (or (mwim-first-position rest position)
                   fallback-position
                   pos)
             (mwim-next-position rest position
                                 (or fallback-position pos)))))))))

(defun mwim-move-to-next-position (expressions)
  "Move point to position defined after evaluating the first expression.
If the point is already there, move to the position defined after
evaluating the second expression from the list of EXPRESSIONS, etc."
  (let ((pos (mwim-next-position expressions)))
    (when pos (goto-char pos))))

(defmacro mwim-goto-next-position (&rest expressions)
  "Wrapper for `mwim-move-to-next-position'."
  (declare (indent 0))
  `(mwim-move-to-next-position ',expressions))


;;; Position functions

(defun mwim-current-comment-beginning ()
  "Return position of the beginning of the current comment.
Return nil, if not inside a comment."
  (let ((syn (syntax-ppss)))
    (and (nth 4 syn)
         (nth 8 syn))))

(defun mwim-line-comment-beginning ()
  "Return position of the beginning of comment on the current line.
Return nil, if there is no comment beginning on the current line."
  (let ((beg (save-excursion
               (end-of-line)
               (or (mwim-current-comment-beginning)
                   ;; There may be a marginal block comment, see
                   ;; <https://github.com/alezost/mwim.el/issues/3>.
                   (progn
                     (skip-chars-backward " \t")
                     (backward-char)
                     (mwim-current-comment-beginning))))))
    (and beg
         (<= (line-beginning-position) beg)
         beg)))

;; For using in `mwim-define-command' macro to generate commands to move
;; to the beginning of comment.
(defalias 'mwim-comment-beginning #'mwim-line-comment-beginning)

(defun mwim-line-beginning ()
  "Return position in the beginning of line.
Use `mwim-beginning-of-line-function'."
  (mwim-point-at (mwim-beginning-of-line)))

(defun mwim-code-beginning ()
  "Return position in the beginning of code."
  (mwim-point-at (mwim-beginning-of-code)))

(defun mwim-line-end ()
  "Return position in the end of line.
Use `mwim-end-of-line-function'."
  (mwim-point-at (mwim-end-of-line)))

(defun mwim-code-end ()
  "Return position in the end of code."
  (mwim-point-at (mwim-end-of-code)))


;;; Moving commands

(defvar mwim-beginning-expressions
  '((mwim-code-beginning)
    (mwim-line-beginning))
  "List of expressions used by `\\[mwim-beginning]' command.
Each expression should return either a number (point position) or
nil (if this position should be skipped) after evaluating.")

(defvar mwim-end-expressions
  '((mwim-code-end)
    (mwim-line-end))
  "List of expressions used by `\\[mwim-end]' command.
See also `mwim-beginning-expressions'.")

;;;###autoload
(defun mwim-beginning ()
  "Move point to a beginning position.
Move to the next position defined by `mwim-beginning-expressions'.
See `mwim-move-to-next-position' for details."
  (interactive "^")
  (mwim-move-to-next-position mwim-beginning-expressions))

;;;###autoload
(defun mwim-end ()
  "Move point to an end position.
Move to the next position defined by `mwim-end-expressions'.
See `mwim-move-to-next-position' for details."
  (interactive "^")
  (mwim-move-to-next-position mwim-end-expressions))

(defun mwim-beginning-of-comment ()
  "Move point to the beginning of comment on the current line.
If the comment does not exist, do nothing."
  (interactive "^")
  (let ((comment-beg (mwim-line-comment-beginning)))
    (when comment-beg
      (goto-char comment-beg))))

(defun mwim-beginning-of-line ()
  "Move point to the beginning of line.
Use `mwim-beginning-of-line-function'."
  (interactive "^")
  (if (functionp mwim-beginning-of-line-function)
      (funcall mwim-beginning-of-line-function)
    (beginning-of-line)))

(defun mwim-end-of-line ()
  "Move point to the end of line.
Use `mwim-end-of-line-function'."
  (interactive "^")
  (if (functionp mwim-end-of-line-function)
      (funcall mwim-end-of-line-function)
    (end-of-line)))

(defun mwim-beginning-of-code ()
  "Move point to the first non-whitespace character on the current line."
  (interactive "^")
  (mwim-beginning-of-line)
  (skip-syntax-forward " " (line-end-position)))

(defun mwim-end-of-code ()
  "Move point to the end of code.

'End of code' means before a possible comment and trailing
whitespaces.  Comments are recognized in any mode that sets
`syntax-ppss' properly.

If current line is fully commented (contains only comment), move
to the end of line."
  (interactive "^")
  (mwim-end-of-line)
  (let ((comment-beg (mwim-line-comment-beginning)))
    (when comment-beg
      (let ((eoc (mwim-point-at
                   (goto-char comment-beg)
                   (skip-chars-backward " \t"))))
        (when (< (line-beginning-position) eoc)
          (goto-char eoc)))))
  (skip-chars-backward " \t"))

(defmacro mwim-define-command (position &rest objects)
  "Define `mwim-POSITION-of-OBJECT1-or-OBJECT2-or-...' command.
POSITION is either `beginning' or `end'.
OBJECT1 and OBJECT2 can be `line', `code' or `comment'."
  (let* ((object1     (car objects))
         (direct-fun  (intern (format "mwim-%S-of-%S" position object1)))
         (fun-name    (intern
                       (concat "mwim-" (symbol-name position) "-of-"
                               (mapconcat #'symbol-name objects "-or-")))))
    `(defun ,fun-name (&optional arg)
       ,(concat (format "Move point to the %S of %S."
                        position object1)
                (mapconcat (lambda (object)
                             (format "
If the point is already there, move to the %S of %S."
                                     position object))
                           (cdr objects) "")
                "\n
If ARG is specified, move forward (or backward) this many lines.
See `forward-line' for details.")
       (interactive
        (progn
          (handle-shift-selection)
          (when current-prefix-arg
            (list (prefix-numeric-value current-prefix-arg)))))
       (if (or (null arg) (= 0 arg))
           (mwim-move-to-next-position
            ',(mapcar (lambda (object)
                        `(,(intern (format "mwim-%S-%S" object position))))
                      objects))
         (forward-line arg)
         (,direct-fun)))))

(mwim-define-command beginning line code)
(mwim-define-command beginning code line)
(mwim-define-command beginning code line comment)
(mwim-define-command end line code)
(mwim-define-command end code line)

;;;###autoload (autoload 'mwim-beginning-of-line-or-code "mwim" nil t)
;;;###autoload (autoload 'mwim-beginning-of-code-or-line "mwim" nil t)
;;;###autoload (autoload 'mwim-beginning-of-code-or-line-or-comment "mwim" nil t)
;;;###autoload (autoload 'mwim-end-of-line-or-code "mwim" nil t)
;;;###autoload (autoload 'mwim-end-of-code-or-line "mwim" nil t)

(provide 'mwim)

;;; mwim.el ends here
