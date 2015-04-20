;; -*- lexical-binding: t -*-

;; should these be buffer local?  Evil mode doesn't make command repetition buffer
;; local, so for now these won't be either.
(setq repeatable-motion-forward-func (lambda () nil))
(setq repeatable-motion-backward-func (lambda () nil))

(defun repeat-motion-forward ()
  (interactive)
  (funcall repeatable-motion-forward-func))
(defun repeat-motion-backward ()
  (interactive)
  (funcall repeatable-motion-backward-func))

(defun make-repeatable-motion (motion-func repeat-func reverse-repeat-func)
  (let ((fwd (cond
              ((functionp repeat-func) repeat-func)
              ((symbolp repeat-func) (symbol-function repeat-func))
              (t symbol-function 'ignore)))
        (bkwd (cond
               ((functionp reverse-repeat-func) reverse-repeat-func)
               ((symbolp reverse-repeat-func) (symbol-function reverse-repeat-func))
               (t symbol-function 'ignore))))
    (lambda ()
      (interactive)
      (setq repeatable-motion-forward-func fwd)
      (setq repeatable-motion-backward-func bkwd)
      (funcall motion-func))))

(defun make-repeatable-pair (forward-sym backward-sym)
  (let* ((fname (intern (concat "repeatable-" (symbol-name forward-sym))))
         (bname (intern (concat "repeatable-" (symbol-name backward-sym)))))
    (fset fname (make-repeatable-motion forward-sym forward-sym backward-sym))
    (fset bname (make-repeatable-motion backward-sym backward-sym forward-sym))))


