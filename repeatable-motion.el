;; -*- lexical-binding: t -*-

;; should these be buffer local?  Evil mode doesn't make command repetition buffer
;; local, so for now these won't be either.
(setq repeatable-motion-forward-func (lambda () nil))
(setq repeatable-motion-backward-func (lambda () nil))
(setq repeatable-motion-numeric-arg 1)

(defun repeat-motion-forward (&optional prefix)
  "Repeat the last repeatable motion used, using the original prefix unless a
new one is given"
  (interactive "p")
  (setq current-prefix-arg (if (equal prefix 1)
                               repeatable-motion-numeric-arg
                             prefix))
  (call-interactively repeatable-motion-forward-func))
(defun repeat-motion-backward (&optional prefix)
  "Repeat the last repeatable motion used, using the original prefix unless a
new one is given"
  (interactive "p")
  (setq current-prefix-arg (if (equal prefix 1)
                               repeatable-motion-numeric-arg
                             prefix))
  (call-interactively repeatable-motion-backward-func))

(defun make-repeatable-motion (motion-func repeat-func reverse-repeat-func)
  "Returns a new repeatable version of a given function, which will repeat using the given
repeat and reverse-repeat functions.  repeat-func may be nil to use the motion func given."
  (let ((fwd (cond
              ((functionp repeat-func) repeat-func)
              ((symbolp repeat-func) (symbol-function repeat-func))
              ((null repeat-func) motion-func)
              (t symbol-function 'ignore)))
        (bkwd (cond
               ((functionp reverse-repeat-func) reverse-repeat-func)
               ((symbolp reverse-repeat-func) (symbol-function reverse-repeat-func))
               (t symbol-function 'ignore))))
    (lambda (&optional prefix)
      (interactive "p")
      (setq repeatable-motion-forward-func fwd)
      (setq repeatable-motion-backward-func bkwd)
      (setq repeatable-motion-numeric-arg prefix)
      (setq current-prefix-arg (list prefix))
      (call-interactively motion-func))))

(defun make-repeatable-pair (forward-sym backward-sym)
  "Define a pair of repeatable functions that are opposites of each other.  They
will be named repeatable-<original-name>"
  (let* ((fname (intern (concat "repeatable-" (symbol-name forward-sym))))
         (bname (intern (concat "repeatable-" (symbol-name backward-sym)))))
    (fset fname (make-repeatable-motion forward-sym forward-sym backward-sym))
    (fset bname (make-repeatable-motion backward-sym backward-sym forward-sym))))


