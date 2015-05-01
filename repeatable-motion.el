;;; -*- lexical-binding: t -*-
;;; repeatable-motion.el

;;; Author: William Hatch <willghatch@gmail.com>
;;; Maintainer: William Hatch <willghatch@gmail.com>
;;; Version: 0.0
;;; Homepage: github.com/willghatch/repeatable-motion.el

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


;;; Code:

;; should these be buffer local?  Evil mode doesn't make command repetition buffer
;; local, so for now these won't be either.
(setq -repeatable-motion-forward-func (lambda () (interactive) nil))
(setq -repeatable-motion-backward-func (lambda () (interactive) nil))
(setq -repeatable-motion-numeric-arg 1)

(defun -repeatable-motion/evil-p ()
  "Is evil available?"
  ;; this may not be the best way to go about it, but this is the main
  ;; function I actually use from evil, so...
  (symbol-function 'evil-declare-motion))

(defun repeatable-motion/forward (&optional prefix)
  "Repeat the last repeatable motion used, using the original prefix unless a
new one is given"
  (interactive "p")
  (setq current-prefix-arg (if (equal prefix 1)
                               -repeatable-motion-numeric-arg
                             prefix))
  (call-interactively -repeatable-motion-forward-func))
(defun repeatable-motion/backward (&optional prefix)
  "Repeat the last repeatable motion used, using the original prefix unless a
new one is given"
  (interactive "p")
  (setq current-prefix-arg (if (equal prefix 1)
                               -repeatable-motion-numeric-arg
                             prefix))
  (call-interactively -repeatable-motion-backward-func))

(when (-repeatable-motion/evil-p)
  (evil-declare-motion 'repeatable-motion/forward)
  (evil-declare-motion 'repeatable-motion/backward))

(defun -repeatable-motion/make-symbol-name (orig-sym)
  (intern (concat "repeatable-motion/" (symbol-name orig-sym))))

(defun repeatable-motion/define (base-motion repeat-motion-reverse &optional repeat-motion evil-inclusive name-prefix)
  "Defines a new repeatable version of a given function, named
'repeatable-motion/<original-name>', which will repeat using the given
repeat and reverse-repeat functions.  If repeat-motion is given, it
will be used for repeating instead of the base motion given.  If
name-prefix is given, it will be used instead of 'repeatable-motion/'
in the new name.  If the evil package is available, motions will be
declared to work well with evil, and definitions where evil-inclusive
is non-nil will cause the motions to have the inclusive property set
for evil."
  (let ((name (-repeatable-motion/make-symbol-name base-motion))
        (repeat-fwd (if repeat-motion repeat-motion base-motion)))
    (fset name
          (lambda (&optional prefix)
            (interactive "p")
            (setq -repeatable-motion-forward-func repeat-fwd)
            (setq -repeatable-motion-backward-func repeat-motion-reverse)
            (setq -repeatable-motion-numeric-arg prefix)
            (setq current-prefix-arg (list prefix))
            (when (-repeatable-motion/evil-p)
              (if evil-inclusive
                  (evil-set-command-property 'repeatable-motion/forward :type 'inclusive)
                (evil-set-command-property 'repeatable-motion/backward :type 'exclusive)))
            (call-interactively base-motion)))
    (when (-repeatable-motion/evil-p)
      (evil-declare-motion name)
      (when evil-inclusive
        (evil-add-command-properties name :type 'inclusive)))))
      

(defun repeatable-motion/define-pair (forward-sym backward-sym)
  "Define a pair of repeatable functions that are opposites of each other.  They
will be named repeatable-motion/<original-name>"
  (repeatable-motion/define forward-sym backward-sym)
  (repeatable-motion/define backward-sym forward-sym))

;; provide some motions
(require 'common-repeatable-motions)
(provide 'repeatable-motion)

;;; repeatable-motion.el ends here
