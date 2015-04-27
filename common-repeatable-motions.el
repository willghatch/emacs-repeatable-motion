
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

(defun -repeatable-def-pair-maybe (fwd bkwd)
  (when (symbol-function fwd)
    (define-repeatable-pair fwd bkwd)))

(defun -repeatable-def-single-maybe (motion-sym f-sym b-sym)
  (when (symbol-function motion-sym)
    (fset (intern (concat "repeatable-" (symbol-name motion-sym)))
          (make-repeatable-motion motion-sym f-sym b-sym))))

(-repeatable-def-pair-maybe 'evil-next-line 'evil-previous-line)
(-repeatable-def-pair-maybe 'evil-next-visual-line 'evil-previous-visual-line)
(-repeatable-def-pair-maybe 'forward-char 'backward-char)
(-repeatable-def-pair-maybe 'forward-word 'backward-word)
(-repeatable-def-pair-maybe 'forward-sentence 'backward-sentence)
(-repeatable-def-pair-maybe 'forward-symbol 'backward-symbol)
(-repeatable-def-pair-maybe 'evil-forward-word-begin 'evil-backward-word-begin)
(-repeatable-def-pair-maybe 'evil-forward-WORD-begin 'evil-backward-WORD-begin)
(-repeatable-def-pair-maybe 'evil-forward-word-end 'evil-backward-word-end)
(-repeatable-def-pair-maybe 'evil-forward-WORD-end 'evil-backward-WORD-end)
(-repeatable-def-pair-maybe 'evil-forward-little-word-begin 'evil-backward-little-word-begin)
(-repeatable-def-pair-maybe 'evil-forward-little-word-end 'evil-backward-little-word-end)
(-repeatable-def-pair-maybe 'evil-forward-char 'evil-backward-char)
(-repeatable-def-pair-maybe 'evil-forward-paragraph 'evil-backward-paragraph)
(-repeatable-def-pair-maybe 'evil-forward-sentence 'evil-backward-sentence)
(-repeatable-def-pair-maybe 'evil-forward-section-begin 'evil-backward-section-begin)
(-repeatable-def-pair-maybe 'evil-forward-section-end 'evil-backward-section-end)
(-repeatable-def-pair-maybe 'evil-previous-open-paren 'evil-next-open-paren)
(-repeatable-def-pair-maybe 'evil-previous-close-paren 'evil-next-close-paren)
(-repeatable-def-pair-maybe 'evil-previous-open-brace 'evil-next-open-brace)
(-repeatable-def-pair-maybe 'evil-previous-close-brace 'evil-next-close-brace)
(-repeatable-def-pair-maybe 'evil-jump-forward 'evil-jump-backward)
(-repeatable-def-pair-maybe 'evil-forward-arg 'evil-backward-arg)

(-repeatable-def-single-maybe 'evil-find-char 'evil-repeat-find-char 'evil-repeat-find-char-reverse)
(-repeatable-def-single-maybe 'evil-find-char-reverse 'evil-repeat-find-char-reverse 'evil-repeat-find-char)
(-repeatable-def-single-maybe 'evil-find-char-to 'evil-repeat-find-char 'evil-repeat-find-char-reverse)
(-repeatable-def-single-maybe 'evil-find-char-to-backward 'evil-repeat-find-char-reverse 'evil-repeat-find-char)
(-repeatable-def-single-maybe 'evil-search-forward 'evil-search-next 'evil-search-previous)
(-repeatable-def-single-maybe 'evil-search-backward 'evil-search-previous 'evil-search-next)
(-repeatable-def-single-maybe 'evil-search-word-forward 'evil-search-next 'evil-search-previous)
(-repeatable-def-single-maybe 'evil-search-word-backward 'evil-search-previous 'evil-search-next)

(provide 'common-repeatable-motions)
