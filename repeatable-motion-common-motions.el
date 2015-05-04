
;;; This is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; <http://www.gnu.org/licenses/>


;;; My strategy here is to define repeatable versions of known motion functions if
;;; the original version is already defined.

(defun repeatable-motion--def-pair-maybe (fwd bkwd)
  (when (symbol-function fwd)
    (repeatable-motion-define-pair fwd bkwd)))

(defun repeatable-motion--def-single-maybe (motion-sym b-sym &optional f-sym evil-inclusive)
  (when (symbol-function motion-sym)
    (repeatable-motion-define motion-sym b-sym f-sym evil-inclusive)))

(repeatable-motion--def-pair-maybe 'forward-char 'backward-char)
(repeatable-motion--def-pair-maybe 'forward-word 'backward-word)
(repeatable-motion--def-pair-maybe 'forward-sentence 'backward-sentence)
(repeatable-motion--def-pair-maybe 'forward-symbol 'backward-symbol)

(eval-after-load 'evil
  '(progn
     (repeatable-motion-define-pair 'evil-next-line 'evil-previous-line)
     (repeatable-motion-define-pair 'evil-next-visual-line 'evil-previous-visual-line)
     (repeatable-motion-define-pair 'evil-forward-word-begin 'evil-backward-word-begin)
     (repeatable-motion-define-pair 'evil-forward-WORD-begin 'evil-backward-WORD-begin)
     (repeatable-motion-define-pair 'evil-forward-word-end 'evil-backward-word-end)
     (repeatable-motion-define-pair 'evil-forward-WORD-end 'evil-backward-WORD-end)
     (repeatable-motion-define-pair 'evil-forward-little-word-begin 'evil-backward-little-word-begin)
     (repeatable-motion-define-pair 'evil-forward-little-word-end 'evil-backward-little-word-end)
     (repeatable-motion-define-pair 'evil-forward-char 'evil-backward-char)
     (repeatable-motion-define-pair 'evil-forward-paragraph 'evil-backward-paragraph)
     (repeatable-motion-define-pair 'evil-forward-sentence-begin 'evil-backward-sentence-begin)
     (repeatable-motion-define-pair 'evil-forward-section-begin 'evil-backward-section-begin)
     (repeatable-motion-define-pair 'evil-forward-section-end 'evil-backward-section-end)
     (repeatable-motion-define-pair 'evil-previous-open-paren 'evil-next-close-paren)
     (repeatable-motion-define-pair 'evil-previous-open-brace 'evil-next-close-brace)
     (repeatable-motion-define-pair 'evil-jump-forward 'evil-jump-backward)
     (repeatable-motion-define-pair 'evil-forward-arg 'evil-backward-arg)

     (repeatable-motion-define 'evil-find-char 'evil-repeat-find-char-reverse 'evil-repeat-find-char t)
     (repeatable-motion-define 'evil-find-char-backward 'evil-repeat-find-char-reverse 'evil-repeat-find-char t)
     (repeatable-motion-define 'evil-find-char-to 'evil-repeat-find-char-reverse 'evil-repeat-find-char t)
     (repeatable-motion-define 'evil-find-char-to-backward 'evil-repeat-find-char-reverse 'evil-repeat-find-char t)
     (repeatable-motion-define 'evil-search-forward 'evil-search-previous 'evil-search-next)
     (repeatable-motion-define 'evil-search-backward 'evil-search-previous 'evil-search-next)
     (repeatable-motion-define 'evil-search-word-forward 'evil-search-previous 'evil-search-next)
     (repeatable-motion-define 'evil-search-word-backward 'evil-search-previous 'evil-search-next)
     ))

(provide 'repeatable-motion-common-motions)
