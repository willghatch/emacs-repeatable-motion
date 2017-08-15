[![MELPA](http://melpa.org/packages/repeatable-motion-badge.svg)](http://melpa.org/#/repeatable-motion)

repeatable-motion.el
====================

The rationale for this package is that it can be nice to have a somewhat obscure motion function that you call by name, or by a longer-than-normal key sequence.  But it would be nice to be able to repeat the function again with a single key.  So here's how it works:

    (require 'repeatable-motion)

    ;; The following will define the function "repeatable-motion-forward-foo",
    ;; which will be the same as forward-foo, but will set it to be repeated.
    (repeatable-motion-define 'forward-foo 'backward-foo)
    ;; keyword arguments:
    ;; :repeat (in case you want a different
    ;;     function to be repeated, eg. repeat-isearch)
    ;; :inclusive (whether the motion should be treated as inclusive by evil-mode)

    ;; This will define two functions: repeatable-motion-fwd-foo and repeatable-motion-bkwd-foo.
    ;; Useful for simple motions that have opposites.
    (define-repeatable-pair 'fwd-foo 'bkwd-foo)

After the above, you can run `M-x repeatable-fwd-foo` to go forword.  Then run `M-x repeatable-motion-forward` or `M-x repeatable-motion-backward` to repeat as much as you want.  If the original call to the repeatable motion had a prefix argument, the repeats use the same one unless another one is given (except 1...).

Some common motions that I know have repeatable versions defined if the original ones are defined.  But I don't want to do any re-binds to not change peoples' workflows without their intervention.  If you think this is not a good idea, I might be convinced to change that.

Use cases
---------

If you are an Evil user, I recommend binding repeatable-motion-forward and backward to the keys you would otherwise use for evil-repeat-find-char and evil-repeat-find-char-reverse (probably ; and ,).  I find that while I want to use evil-repeat-search with other motions in between, but I only repeat a find-char if I didn't get to the one I wanted first.

Personally, I have in my evil-normal-state two keys bound to prefix commands which are full of motions (forward for one, and backward for the other).  I try not to use forward-char and use more efficient motions, but on occasion it is easier to simply repeat forward-char.  This lets me lose the single-key binding to forward-char and just use a two-key binding, but then repeat it (or anything else) with a single key.

Install
-------

Requires emacs version 24.  Install through Melpa or clone the repo on your load path and `(require 'repeatable-motion)`.

Configure
---------

- Customize `repeatable-motion-define-common-motions-p` to `nil` before loading if you don't want motions to be defined automatically.
- Customize `repeatable-motion-definition-prefix` to whatever prefix you want for the repeatable versions.  Defaults to "repeatable-motion-".  The default conforms to emacs packaging standards, but frankly I recommend something short like "rmo/".
- Customize `repeatable-motion-count-needed-prefix` to whatever prefix you want for versions that are repeatable only when they receive a non-1 prefix argument.  These motions will be like those provided with eg. repmo.vim.  Defaults to `nil` (meaning they won't be defined at all).  A good one may be "rmo-c/".
- Customize `repeatable-motion-training-wheels-p` to `t` if you want training-wheels mode -- repeated calls to the motion will fail, and tell you to use the repeat key instead.
- Customize `repeatable-motion-training-wheels-timeout` a number of seconds for the training-wheels restriction to wear off.

Examples
--------

You can put this in your emacs config file:

```
;; set or customize these before requiring the package for them to take effect
;; for automatically defined movements.
(setq repeatable-motion-definition-prefix "rmo/")
(setq repeatable-motion-count-needed-prefix "rmo-c/")
(require 'repeatable-motion)

;; when we require evil, some common motions will be defined.
;; Since we set the prefix to `rmo/`, we will have functions defined
;; like `rmo/evil-next-line`, and `rmo/evil-forward-WORD-end`.
(require 'evil)
(define-key evil-motion-state-map ";" 'repeatable-motion-forward)
(define-key evil-motion-state-map "," 'repeatable-motion-backward)
;; Let's make j/k be repeatable, but only when we use a prefix (eg `9j`)
(define-key evil-motion-state-map "j" 'rmo-c/evil-next-line)
(define-key evil-motion-state-map "k" 'rmo-c/evil-previous-line)
;; Let's add some more common movements, but make them repeatable always
(define-key evil-motion-state-map "w" 'rmo/evil-forward-word-begin)
(define-key evil-motion-state-map "b" 'rmo/evil-backward-word-begin)

;; Let's define a new repeatable motion and bind it
(repeatable-motion-define-pair 'org-forward-element 'org-backward-element)
;; Now rmo/org-forward-element and rmo/org-backward-element are both defined,
;; as well as rmo-c/org-forward-element and rmo-c/org-backward-element.

;; Let's use `h` and `l` as prefixes for motions -- `h` for backward, `l` for forward.
(define-key evil-motion-state-map "l" nil)
(define-key evil-motion-state-map "h" nil)
(define-key evil-motion-state-map "lc" 'rmo/evil-forward-char)
(define-key evil-motion-state-map "hc" 'rmo/evil-backward-char)
(define-key evil-motion-state-map "le" 'rmo/org-forward-element)
(define-key evil-motion-state-map "he" 'rmo/org-backward-element)
(define-key evil-motion-state-map "lp" 'rmo/evil-forward-paragraph)
(define-key evil-motion-state-map "hp" 'rmo/evil-backward-paragraph)
;; etc

;; Now we can type `le` to move forward one org element, then type `;` to move another.
;; If we then type `j`, `;` still moves forward one element, since we used `rmo-c/` for j/k.
;; If we type `5j`, ';' will repeat `5j` instead of org-forward-element.
```

To see more examples of defining motions, as well as to see which ones are defined by default, see `repeatable-motion-common-motions.el`.


